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

    // auto res = solveEasy(input);
    auto res = solveHard(input);

    res.writeln;
}

Tuple!(int[], int[])[] parseInput(string[] input) {
    Tuple!(int[], int[]) parseLine(string input) {
        auto cardNumberAndCards = input.split(":");
        auto arrays = cardNumberAndCards[1].split("|");
        auto toIntArray = (string s) => s.split.map!(to!int).array;
        auto firstArr = toIntArray(arrays[0]);
        auto secondArr = toIntArray(arrays[1]);

        return tuple(firstArr, secondArr);
    }

    return input.map!parseLine.array;
}

int countMatches(Tuple!(int[], int[]) a) =>
    setIntersection(a[0].sort, a[1].sort).walkLength;

int solveEasy(string[] input) {
    int countToScore(int cnt) => cnt == 0 ? 0 : 2 ^^ (cnt-1);

    return input.parseInput.map!(pipe!(countMatches, countToScore)).sum;
}

int countsToScore(int[] counts) {
    int sum;
    auto toAdd = new int[] (counts.length + 1);
    auto currentSz = 1;
    foreach (i, cnt; counts) {
        currentSz += toAdd[i];

        sum += currentSz * cnt + 1;

        toAdd[i+1] += currentSz;
        toAdd[i+1+cnt] -= currentSz;
    }

    return sum;
}

int solveHard(string[] input) => input.parseInput.map!countMatches.array.countsToScore;