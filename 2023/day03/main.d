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
    while ((line = readln()) !is null) { input ~= line.strip; }

    auto res = solveEasy(input);
    // auto res = solveHard(input);

    res.writeln;
}

int solveEasy(string[] input) {
    bool inbounds(int x, int y) => x >= 0 && x < input.length && y >= 0 && y < input[0].length;

    auto sum = 0;

    foreach (i, row; input) {

        bool adjacentSymbol(int col) {
            foreach (adjRw; -1 .. +2) {
                foreach (adjCol; -1 .. +2) {
                    if (abs(adjRw) + abs(adjCol) == 0) { continue; }
                    int x = i + adjRw;
                    int y = col + adjCol;
                    if (!inbounds(x, y)) { continue; }

                    if (!input[x][y].isDigit && input[x][y] != '.') { return true; }
                }
            }

            return false;
        }

        for (int start = 0, end; start < row.length; start = max(start+1, end)) {
            end = start;
            while (end < row.length && row[end].isDigit) { ++end; }

            if (start == end) { continue; }

            auto include = iota(start, end).any!adjacentSymbol;
            if (!include) { continue; }

            auto val = row[start .. end].to!int;
            sum += val;
        }
    }

    return sum;
}