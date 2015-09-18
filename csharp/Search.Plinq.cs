using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace Search
{
    class SearchPlinq
    {
        internal static void Search(string folder)
        {
            var knicks = new Regex("(?i)knicks", RegexOptions.CultureInvariant);
            var hoods = Directory.EnumerateFiles(Path.Combine(folder, "tweets"))
                .AsParallel()
                .SelectMany(f => File.ReadLines(f))
                .Where(l => knicks.IsMatch(l))
                .GroupBy(l => l.Split('\t')[1])
                .OrderByDescending(g => g.Count())
                .ThenBy(g => g.Key);

            File.WriteAllLines(Path.Combine(folder, "cs_plinq_output"),
                hoods.Select(h => $"{h.Key}\t{h.Count()}"));
        }
    }
}
