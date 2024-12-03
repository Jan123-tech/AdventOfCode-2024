﻿var items = File.ReadAllLines("data.txt")
	.Select(x => x.Split(" "))
	.Select(x => x.Select(y => int.Parse(y)))
	.Select(x => x.Select ((_, i) => x.Where((_, j) => j != i)))
	.Select(items => 
	{
		return items
			.Select(x => (x.Take(x.Count() - 1), x.Skip(1)))
			.Select(x => x.Item1.Zip(x.Item2))
			.Select(x => x.Select(y => y.Second - y.First))
			.Select(x => x.Select(y => (diff: y, absDiff: Math.Abs(y))));
	});

var valid = items.Where(items0 => items0.Where(x =>
	x.All(y => y.absDiff > 0 && y.absDiff <= 3) &&
		(x.All(y => y.diff < 0) || x.All(y => y.diff > 0))).Any());

Console.WriteLine(valid.Count());