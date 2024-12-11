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

alias Seeds = long[];
alias MapEntry = Tuple!(long, "dst", long, "src", long, "cnt");
alias Map = MapEntry[];
alias MapFamily = Map[];
alias State = Tuple!(Seeds, "seeds", MapFamily, "maps");

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    debug { input[0 .. 4].writeln; }

    auto state = input.parse;

    debug { state.maps[0].writeln; }

    // auto res = solveEasy(state);
    auto res = solveHard(state);

    res.writeln;
}

Seeds parseSeeds(string line) {
    long[] arr;
    line.formattedRead("seeds: %(%s %)", arr);
    return arr;
}

MapEntry parseMapEntry(string input) {
    MapEntry mp;
    input.formattedRead("%s %s %s", mp.dst, mp.src, mp.cnt);
    return mp;
}

Map parseMap(string[] input) => input[1..$].map!parseMapEntry.array;

MapFamily parseMapFamily(string[] input) => input.split("").map!parseMap.array;

State parse(string[] input) {
    auto seeds = input[0].parseSeeds;
    auto mapFamily = input[2..$].parseMapFamily;
    return State(seeds, mapFamily);
}

long toTarget(long seedVal, MapFamily mapFamily) {
    debug { writeln("seedval: ", seedVal); }
    auto current = seedVal;
    outer: foreach (mp; mapFamily) {
        foreach (mpEntry; mp) {
            if (mpEntry.src <= current && current < mpEntry.src + mpEntry.cnt) {
                current = mpEntry.dst + current - mpEntry.src;
                debug { writeln("changed to ", current); }
                continue outer;
            }
        }
    }

    return current;
}

long solveEasy(State state) => state.seeds.map!(e => e.toTarget(state.maps)).minElement;

bool isOk(long candidate, State state) {
    auto current = candidate;
    outer: foreach_reverse (mp; state.maps) {
        foreach (mpEntry; mp) {
            if (mpEntry.dst <= current && current < mpEntry.dst + mpEntry.cnt) {
                current = mpEntry.src + current - mpEntry.dst;
                continue outer;
            }
        }
    }

    for (int i = 0; i < state.seeds.length; i += 2) {
        auto start = state.seeds[i];
        auto len = state.seeds[i + 1];
        if (start <= current && current < start + len) {
            return true;
        }
    }

    return false;
}

long solveHard(State state) {
    auto mx = 1_000_000_000;

    foreach (candidate; 1 .. mx) {
        debug { if (candidate % 1_000_000 == 0) { candidate.writeln; } }

        if (isOk(candidate, state)) {
            return candidate;
        }
    }

    return mx;
}