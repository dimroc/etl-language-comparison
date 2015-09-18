using System.Linq;

namespace Search
{
    class Program
    {
        static void Main(string[] args)
        {
            var tmpFolder = args.First(a => !a.StartsWith("-"));
            if (args.Any(a => a == "-plinq")) SearchPlinq.Search(tmpFolder);
            else SearchBinary.Search(tmpFolder);
        }
    }
}
