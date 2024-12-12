int min_distance = -1;
int times = 0;
for (int i = 0; i < args.Length; i++)
{
    for (int j = 0; j < args.Length; j++)
    {
        if (i != j)
        {
            int distance = levenshtein(args[i], args[j]);
            if (min_distance == -1 || min_distance > distance)
            {
                min_distance = distance;
            }
            times++;
        }
    }
}
Console.WriteLine($"times: {times}");
Console.WriteLine($"min_distance: {min_distance}");

static int levenshtein(string str1, string str2)
{
    int n = str1.Length;
    int m = str2.Length;
    int[,] matrix = new int[m + 1, n + 1];

    for (int i = 0; i <= m; i++)
    {
        matrix[i, 0] = i;
    }

    for (int j = 0; j <= n; j++)
    {
        matrix[0, j] = j;
    }

    for (int i = 1; i <= m; i++)
    {
        for (int j = 1; j <= n; j++)
        {
            int cost = (str1[i - 1] == str2[j - 1] ? 0 : 1);
            matrix[i, j] = int.Min(
                matrix[i - 1, j] + 1, int.Min(matrix[i, j - 1] + 1,
                          matrix[i - 1, j - 1] + cost));
        }
    }

    return matrix[m, n];
}
