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

alias Pos = Tuple!(int, "x", int, "y");

Pos getStart(string[] input) {
    Pos start;
    foreach (i, rw; input) {
        auto j = rw.indexOf('S');
        if (j != -1) {
            return Pos(i, j);
        }
    }

    assert(false);
}

Tuple!(int, int)[][char] getAdj() => 
    [
        '|': [tuple(-1, 0), tuple(1, 0)],
        '-': [tuple(0, 1), tuple(0, -1)],
        'F': [tuple(0, 1), tuple(1, 0)],
        'J': [tuple(-1, 0), tuple(0, -1)],
        'L': [tuple(-1, 0), tuple(0, 1)],
        '7': [tuple(0, -1), tuple(1, 0)]
    ];

bool inbounds(string[] input, Pos p) => 0 <= p.x && p.x < input.length && 0 <= p.y && p.y < input[0].length;

Pos[] getNxt(string[] input, Pos start) {
    auto adj = getAdj();

    Pos[] res;
    foreach (i; -1 .. +2) {
        foreach (j; -1 .. +2) {
            if (abs(i) + abs(j) != 1) { continue; }

            auto nxt = Pos(start.x + i, start.y + j);
            if (!inbounds(input, nxt)) { continue; }
            if (input[nxt.x][nxt.y] !in adj) { continue; }

            foreach (toAdd; adj[input[nxt.x][nxt.y]]) {
                auto added = Pos(nxt.x + toAdd[0], nxt.y + toAdd[1]);
                if (added == start) { res ~= nxt; }
            }
        }
    }

    return res;
}

int solveEasy(string[] input) {
    auto start = getStart(input);
    auto adj = getAdj();

    auto nxt = getNxt(input, start);

    debug { nxt.writeln; }

    auto len = 1;
    auto fr = start;
    auto cur = nxt[0];
    do {
        auto temp = cur;
        auto nxtCandidates = adj[input[cur.x][cur.y]].map!(t => Pos(cur.x + t[0], cur.y + t[1])).array;
        cur = nxtCandidates[0] != fr ? nxtCandidates[0] : nxtCandidates[1];
        fr = temp;
        len += 1;
        // debug { writeln(fr, ' ', cur); }
    } while (input[cur.x][cur.y] != 'S');

    return (len + 1) / 2;
}

int solveHard(string[] input) {
    auto start = getStart(input);
    auto adj = getAdj();

    auto nxt = getNxt(input, start);

    auto dirs = [tuple(0, 1), tuple(1, 0), tuple(0, -1), tuple(-1, 0)];

    auto fr = start;
    auto cur = nxt[0];
    auto curdir = tuple(cur.x - start.x, cur.y - start.y);
    do {
        auto temp = cur;
        auto nxtCandidates = adj[input[cur.x][cur.y]].map!(t => Pos(cur.x + t[0], cur.y + t[1])).array;
        cur = nxtCandidates[0] != fr ? nxtCandidates[0] : nxtCandidates[1];
        fr = temp;
        
    } while (input[cur.x][cur.y] != 'S');

    return 0;
}