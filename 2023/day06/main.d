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

alias ResultType = long;

alias Times = int[];
alias Distances = int[];
alias Race = Tuple!(long, "time", long, "distance");
alias State = Tuple!(Times, "times", Distances, "distances");

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    // auto res = solveEasy(input);
    auto res = solveHard(input);

    res.writeln;
}

int[] parseArrayLine(string input) => input.split(":")[1].split.map!(to!int).array;

Times parseTimes(string input) => input.parseArrayLine;

Distances parseDistances(string input) => input.parseArrayLine;

State parseEasy(string[] input) {
    auto ts = input[0].parseTimes;
    auto ds = input[1].parseDistances;
    return State(ts, ds);
}

ResultType numberOfWaysToBeat(Race race) 
    =>
    iota(1, race.time)
    .count!(speed => speed * (race.time - speed) > race.distance);

Race[] toRaces(State state) => zip(state.times, state.distances).map!Race.array;

ResultType solveEasy(string[] input)
    =>
    input.parseEasy
    .toRaces
    .map!numberOfWaysToBeat
    .fold!((a, b) => a * b);

long parseHardLine(string input) => input.split(":")[1].filter!isDigit.to!long;

Race parseHard(string[] input) {
    auto time = input[0].parseHardLine;
    auto distance = input[1].parseHardLine;
    return Race(time, distance);
}

BigInt getDist(Race r, long speed) => speed.to!BigInt * (r.time - speed);

ResultType solveHard(string[] input) {
    auto race = input.parseHard;
    debug { race.writeln; }
    
    long le = 1, r = race.time - 1;
    while (le < r) {
        auto m = (le + r) / 2;
        if (getDist(race, m) < getDist(race, m + 1)) {
            le = m + 1;
        } else {
            r = m;
        }
    }

    auto peak = le;
    debug { iota(peak-1, peak+2).map!(speed => getDist(race, speed)).writeln; }

    if (getDist(race, peak) <= race.distance.to!BigInt) {
        return 0;
    }

    le = 1, r = peak;
    while (le < r) {
        auto m = (le + r) / 2;
        if (getDist(race, m) <= race.distance) {
            le = m + 1;
        } else {
            r = m;
        }
    }

    auto rangeL = le;

    le = peak, r = race.time - 1;
    while (le < r) {
        auto m = (le + r) / 2;
        if (getDist(race, m) > race.distance) {
            le = m + 1;
        } else {
            r = m; 
        }
    }

    auto rangeR = le;

    debug { writeln(rangeL, " ", rangeR); }

    return rangeR - rangeL;
}