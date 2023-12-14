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

alias Record = Tuple!(string, "line", int[], "arrangement");

Record parse(string input) {
    string line;
    int[] numbers;
    input.formattedRead("%s %(%s,%)", line, numbers);
    return Record(line, numbers);
}

Record[] parse(string[] input) => input.map!parse.array;

int solve(Record record) {
    auto unknowns = record.line.enumerate(0).filter!(t => t[1] == '?').map!(t => t[0]).array;
    auto rawLine = record.line.dup.replace('?', '.');
    auto good = 0;

    bool isGucci(char[] line)
        => 
        line.group.filter!(t => t[0] == '#').map!(t => t[1]).array == record.arrangement;

    bool isCorrect(int hash) {
        auto curLine = rawLine.dup;
        auto bits = hash.bitsSet;
        foreach (idx; bits) { curLine[unknowns[idx]] = '#'; }

        return isGucci(curLine);
    }

    return iota(0, 2 ^^ unknowns.length).count!isCorrect;
}

int solve(Record[] records) => records.map!solve.sum;

int solveEasy(string[] input) => input.parse.solve;

int solveHard(string[] input) => 0;