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

immutable int repeatTimes = 5;

Record extend(Record record) 
    => 
    Record(record.line.repeat(repeatTimes).join('?'), record.arrangement.repeat(repeatTimes).join);

Record[] parseHard(string[] input) => input.map!parse.map!extend.array;

bool canBeOperational(char c) => c == '.' || c == '?';

long solveHard(Tuple!(int, Record) t) {
    auto index = t[0];
    auto record = t[1];
    debug { record.writeln; }

    bool canBeEndOfLastBlock(int stringPos, int arrayPos) {
        int required = record.arrangement[arrayPos-1];
        return stringPos - required >= 0 &&
            record.line[stringPos-required .. stringPos].all!(c => c == '#' || c == '?') &&
            (stringPos - required == 0 || record.line[stringPos-required-1].canBeOperational);
    }

    long ways(int solutionIndex, int stringPos, int arrayPos) {
        if (stringPos == -1 || stringPos == 0) {
            return arrayPos == 0;
        }

        if (arrayPos == 0) {
            return record.line[0 .. stringPos].all!(c => c != '#');
        }

        if (record.line[stringPos-1] == '.') {
            return memoize!ways(solutionIndex, stringPos-1, arrayPos);
        }

        if (record.line[stringPos-1] == '#') {
            if (canBeEndOfLastBlock(stringPos, arrayPos)) {
                return memoize!ways(solutionIndex, stringPos - record.arrangement[arrayPos-1] - 1, arrayPos - 1);
            }

            return 0;
        }

        auto operationalBlock = memoize!ways(solutionIndex, stringPos-1, arrayPos);
        auto damagedBlock = canBeEndOfLastBlock(stringPos, arrayPos) ?
            memoize!ways(solutionIndex, stringPos - record.arrangement[arrayPos-1] - 1, arrayPos - 1) :
            0;

        return operationalBlock + damagedBlock;
    }

    return ways(index, record.line.length, record.arrangement.length);
}

long solveHard(Record[] records) => records.enumerate(1).map!solveHard.sum;

long solveHard(string[] input) => input.parseHard.solveHard; 