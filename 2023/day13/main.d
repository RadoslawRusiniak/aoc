import core.bitop, std.bitmanip;
import core.checkedint;
import core.time;
import std.algorithm, std.functional, std.meta;
import std.array, std.container;
import std.bigint;
import std.conv;
import std.format;
import std.math, std.numeric;
import std.range, std.range.interfaces;
import std.stdio, std.string;
import std.ascii, std.typecons;
import std.datetime.date, std.datetime.systime;

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    auto res = solveEasy(input);
    // auto res = solveHard(input);

    res.writeln;
}

bool isHorizontalReflection(string[] input, int x) {
    auto res = input[x..$].zip(input[0..x].retro).all!(t => t[0] == t[1]);

    debug { writeln(input, ' ', x, ' ', res); }
    return res;
} 

int gethorizontalReflectionsScore(string[] input)
    => 
    iota(1, input.length)
    .filter!(x => isHorizontalReflection(input, x))
    .sum;

int getScore(string[] input) => 
    input.gethorizontalReflectionsScore * 100 +
    input.map!(to!(char[])).array.transposed.map!(to!string).array.gethorizontalReflectionsScore;

int solveEasy(string[] input) => input.split("").map!getScore.sum;

int solveHard(string[] input) {
    return 0;
}