import core.bitop, std.bitmanip;
import core.checkedint;
import core.time;
import std.algorithm, std.functional, std.meta;
import std.array, std.container;
import std.bigint;
import std.conv;
import std.math, std.numeric;
import std.range, std.range.interfaces;
import std.stdio, std.string;
import std.ascii, std.typecons;
import std.datetime.date, std.datetime.systime;

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln()) !is null) { input ~= line; }

    // auto res = solveEasy(input);
    auto res = solveHard(input);

    res.writeln;
}

int getDigit(string s) => s.find!isDigit.front - '0';

int getNumberEasy(ref string ln) => ln.getDigit * 10 + ln.dup.retro.to!string.getDigit;

int solveEasy(string[] input) => input.map!getNumberEasy.sum;

int getFirstNumber(string input, string function(string) modifier) {
    auto numbers = 
        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        .map!modifier;

    auto searchStr = modifier(input);

    foreach (i; 0 .. searchStr.length) {
        if (searchStr[i].isDigit) {
            return searchStr[i] - '0';
        }

        foreach (value, name; numbers.enumerate(1)) {
            if (searchStr[i..$].startsWith(name)) {
                return value;
            }
        }
    }

    assert(false);
}

int getNumberHard(string input) => 
    getFirstNumber(input, s => s) * 10 + getFirstNumber(input, s => s.dup.retro.to!string);

int solveHard(string[] input) => input.map!getNumberHard.sum;