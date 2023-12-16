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
    char[][] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line.map!(to!char).array; }

    debug { input.each!writeln; }

    // auto res = solveEasy(input);
    auto res = solveHard(input);

    res.writeln;
}

void moveUp(char[][] input) {
    int[] colMin = new int[] (input.length);
    foreach (i, rw; input) {
        foreach (j, e; rw) {
            if (e == '#') { colMin[j] = i+1; }
            if (e == 'O') {
                input[i][j] = '.';
                input[colMin[j]][j] = 'O';
            
                ++colMin[j];
            }
        }
    }
}

long getAns(char[][] input) {
    debug { writeln; input.each!writeln; }

    auto allLen = input.length;
    auto ans = 0;
    foreach (i, rw; input) {
        foreach (e; rw) {
            if (e == 'O') {
                auto cur = allLen - i;
                ans += cur;
            }
        }
    }

    return ans;
}

long solveEasy(char[][] input) {
    moveUp(input);
    
    debug { writeln; input.each!writeln; }

    return getAns(input);
}

void rotateRight(ref char[][] input) { 
    input = input.transposed.map!array.array.to!(char[][]);

    input = input.map!reverse.array;
}

long solveHard(char[][] input) {
    immutable long cycles = 1_000;//_000_000;
    foreach (i; 0 .. cycles * 4) {
        moveUp(input);
        rotateRight(input);
    }

    return getAns(input);
}

unittest {
    auto arr = [['a','b'], ['c','d']];
    auto expected = [['c','a'], ['d','b']];
    rotateRight(arr);

    debug { arr.writeln; }
    debug { expected.writeln; }
    
    assert(arr == [['c','a'], ['d','b']]);
}