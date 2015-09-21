using System.Collections.Concurrent;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Search
{
    class SearchBinary
    {
        const int BufSize = 2 * 64 * 1024;
        static byte[] knicks = Encoding.ASCII.GetBytes("knicks");

        internal static void Search(string folder)
        {
            var hoods = new ConcurrentBag<string>();

            Parallel.ForEach(Directory.EnumerateFiles(Path.Combine(folder, "tweets")), f =>
            {
                using (var fs = new FileStream(f, FileMode.Open))
                {
                    var buf = new byte[BufSize];
                    var bufOffset = 0; // offset into the buffer of the chunk currently read (0 or BufSize / 2)
                    var start = 0; // offset into the buffer where the current field started
                    var cur = 0; // number of bytes in current field
                    var field = 0; // field number
                    var hoodStart = 0; // offset into the buffer where the hood field started
                    var hoodCount = 0; // number of bytes in hood field

                    for (;;)
                    {
                        var read = fs.Read(buf, bufOffset, BufSize / 2);
                        if (read == 0) break;

                        for (var i = 0; i < read; i++, cur++)
                        {
                            var b = buf[bufOffset + i];
                            if (b == '\t' || b == '\n')
                            {
                                if (field == 1)
                                {
                                    hoodStart = start;
                                    hoodCount = cur - 1;
                                }
                                else if (field == 3)
                                {
                                    if (NaiveSearch(buf, start, start + cur))
                                    {
                                        var hood = (hoodStart + hoodCount) <= BufSize ?
                                            Encoding.UTF8.GetString(buf, hoodStart, hoodCount) :
                                            Encoding.UTF8.GetString(buf, hoodStart, BufSize - hoodStart)
                                            + Encoding.UTF8.GetString(buf, 0, (hoodStart + hoodCount) - BufSize);
                                        hoods.Add(hood);
                                    }
                                }

                                start = bufOffset + i + 1;
                                cur = 0;
                                field = (field + 1) % 4;
                            }
                        }

                        bufOffset = (bufOffset + BufSize / 2) % BufSize;
                    }
                }
            });

            var counts = hoods.GroupBy(t => t)
                .OrderByDescending(g => g.Count())
                .ThenBy(g => g.Key)
                .Select(h => $"{h.Key}\t{h.Count()}");

            File.WriteAllLines(Path.Combine(folder, "cs_binary_output"), counts);

        }

        static bool NaiveSearch(byte[] buf, int start, int end)
        {
            int j;

            for (var i = start; i < (end - knicks.Length + 1); i++)
            {
                for (j = 0; j < knicks.Length; j++)
                {
                    var o = (i + j) % BufSize;
                    var b = buf[o] | 0x20;
                    if (b != knicks[j]) break;
                }

                if (j == knicks.Length) return true;
            }

            return false;
        }
    }
}
