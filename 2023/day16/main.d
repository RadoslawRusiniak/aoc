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

alias Dir = Tuple!(int, "x", int, "y");
alias Pos = Tuple!(int, "x", int, "y");

immutable Right = Dir(0, 1);
immutable Left = Dir(0, -1);
immutable Up = Dir(-1, 0);
immutable Down = Dir(1, 0);

int solveEasy(string[] input) {
    Dir[][Pos] vis; 

    bool inbounds(Pos p) => 0 <= p.x && p.x < input.length && 0 <= p.y && p.y < input[0].length;

    Dir[] getNewDir(Dir d, char c) {
        switch (c)
        {
            case '.': 
                return [d];
            case '/':
                if (d == Right) { return [Up]; }
                if (d == Up) { return [Right]; }
                if (d == Left) { return [Down]; }
                if (d == Down) { return [Left]; }
                assert(false);
            case '\\':
                if (d == Right) { return [Down]; }
                if (d == Down) { return [Right]; }
                if (d == Left) { return [Up]; }
                if (d == Up) { return [Left]; }
                assert(false);
            case '-':
                if (d == Right || d == Left) { return [d]; }
                return [Left, Right];
            case '|':
                if (d == Up || d == Down) { return [d]; }
                return [Up, Down];
            default:
                assert(false);
        }
    }

    void go(Pos p, Dir dir) {
        if (p in vis && vis[p].canFind(dir)) { return; }

        vis[p] ~= dir;

        auto dirs = getNewDir(dir, input[p.x][p.y]);
        foreach (d; dirs) {
            auto newPos = Pos(p.x + d.x, p.y + d.y);
            if (inbounds(newPos)) { go(newPos, d); }
        }
    }

    go(Pos(0, 0), Dir(0, 1));

    // debug { vis.byKeyValue.each!(v => writeln(v.key, ' ', v.value)); }

    return vis.keys.length;
}