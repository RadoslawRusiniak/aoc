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

alias CubeInfo = Tuple!(int, "count", string, "color");
alias Draw = CubeInfo[];
alias Game = Tuple!(int, "id", Draw[], "draws");

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln()) !is null) { input ~= line; }

    auto games = parseInput(input);

    debug { games[0 .. 3].each!(g => g.draws[0].writeln); }

    // auto res = solveEasy(games);
    auto res = solveHard(games);

    res.writeln;
}

Game[] parseInput(string[] input) {
    CubeInfo parseCubeInfo(string input) {
        // debug { input.writeln; }
        
        auto splitted = input.strip.split(" ");
        auto cnt = splitted[0].to!int;
        auto clr = splitted[1];
        return CubeInfo(cnt, clr);
    }

    Draw parseDraw(string input) { return input.split(",").map!parseCubeInfo.array; }

    Game parseLine(string input) {
        auto gameNumberAndDraws = input.split(":");

        auto id = gameNumberAndDraws[0]["Game ".length .. $].to!int;

        auto draws = gameNumberAndDraws[1].split(";").map!parseDraw.array;

        return Game(id, draws);
    }

    return input.map!parseLine.array;
}

bool possibleGame(Game game) {
    auto bag = ["red": 12, "green": 13, "blue": 14];

    bool possibleCubeInfo(CubeInfo cubeInfo) => 
        bag.keys.canFind(cubeInfo.color) && cubeInfo.count <= bag[cubeInfo.color];

    bool possibleDraw(Draw draw) => draw.all!possibleCubeInfo;

    return game.draws.all!possibleDraw;
}

int solveEasy(Game[] games) => games.filter!possibleGame.map!(g => g.id).sum;

int powerOfCubesInTheGame(Game game) {
    auto bag = ["red": 0, "green": 0, "blue": 0];
    
    void updateMaxForCubeInfo(CubeInfo cubeInfo) {
        bag[cubeInfo.color] = max(cubeInfo.count, bag[cubeInfo.color]);
    }

    void updateMaxForDraw(Draw draw) { draw.each!updateMaxForCubeInfo; }

    game.draws.each!updateMaxForDraw;

    return bag["red"] * bag["green"] * bag["blue"];
}

int solveHard(Game[] games) => games.map!powerOfCubesInTheGame.sum;