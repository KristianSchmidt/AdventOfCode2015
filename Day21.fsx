#load "Helpers.fsx"

open System

let (hp,dmg,armor) =
    Helpers.Web.getInput 21
    |> Array.map (fun s -> s.Replace("Hit Points: ","")
                            .Replace("Damage: ", "")
                            .Replace("Armor: ", "")
    )
    |> Array.map int
    |> (fun [|a1;a2;a3|] -> (a1,a2,a3))

type Character = { Hp : int; Dmg : int; Armor : int }
type Turn = | Mine | Boss
type Result = | Win | Lose

let me = { Hp = 100; Dmg = 0; Armor = 0 }
let boss = { Hp = hp; Dmg = dmg; Armor = armor }

let rec fight myChar bossChar turn =
    match turn with
    | Mine ->
        let dmg = max 1 (myChar.Dmg - bossChar.Armor)
        if (bossChar.Hp < dmg) then
            Win
        else
            fight myChar { bossChar with Hp = bossChar.Hp - dmg } Boss
    | Boss ->
        let dmg = max 1 (bossChar.Dmg - myChar.Armor)
        if (myChar.Hp < dmg) then
            Lose
        else
            fight { myChar with Hp = myChar.Hp - dmg } bossChar Mine


fight me boss Mine
fight { Hp = 8; Dmg = 5; Armor = 5 } { Hp = 12; Dmg = 7; Armor = 2 } Mine

type Item = { Cost : int; Dmg : int; Armor : int }

let weapons = [
        (*
        Dagger        8     4       0
        Shortsword   10     5       0
        Warhammer    25     6       0
        Longsword    40     7       0
        Greataxe     74     8       0
        *)
        { Cost = 8;  Dmg = 4; Armor = 0;}
        { Cost = 10; Dmg = 5; Armor = 0;}
        { Cost = 25; Dmg = 6; Armor = 0;}
        { Cost = 40; Dmg = 7; Armor = 0;}
        { Cost = 74; Dmg = 8; Armor = 0;}
    ]

let armors = [
        (*
        Leather      13     0       1
        Chainmail    31     0       2
        Splintmail   53     0       3
        Bandedmail   75     0       4
        Platemail   102     0       5
        *)
        { Cost = 0;   Dmg = 0; Armor = 0 }
        { Cost = 13;  Dmg = 0; Armor = 1 }
        { Cost = 31;  Dmg = 0; Armor = 2 }
        { Cost = 53;  Dmg = 0; Armor = 3 }
        { Cost = 75;  Dmg = 0; Armor = 4 }
        { Cost = 102; Dmg = 0; Armor = 5 }
    ]

let rings = [
        (*
        Damage +1    25     1       0
        Damage +2    50     2       0
        Damage +3   100     3       0
        Defense +1   20     0       1
        Defense +2   40     0       2
        Defense +3   80     0       3
        *)
        { Cost = 0;  Dmg = 0; Armor = 0 }
        { Cost = 25;  Dmg = 1; Armor = 0 }
        { Cost = 50;  Dmg = 2; Armor = 0 }
        { Cost = 100; Dmg = 3; Armor = 0 }
        { Cost = 20;  Dmg = 0; Armor = 1 }
        { Cost = 40;  Dmg = 0; Armor = 2 }
        { Cost = 80;  Dmg = 0; Armor = 3 }
]

let rings2 =
    seq { for r in rings do for r1 in rings do if (r <> r1) then yield [r;r1] }
    |> Seq.toList
    |> List.append [[{ Cost = 0;  Dmg = 0; Armor = 0 }]]

let equip (me : Character) (i : Item) =
    { me with Dmg = me.Dmg + i.Dmg; Armor = me.Armor + i.Armor }

let fights =
    seq {
        for w in weapons do
            for a in armors do
                for rs in rings2 do
                    let cost = w.Cost + a.Cost + (List.sumBy (fun r -> r.Cost) rs)
                    let meEquipped =
                        List.collect id [[w];[a];rs]
                        |> List.fold equip me
                    fight meEquipped boss Mine, cost
    }
    |> Seq.toList
    
let ans1 =
    fights
    |> List.filter (fst >> ((=)Win))
    |> List.minBy snd
    |> snd

let ans2 =
    fights
    |> List.filter (fst >> ((=)Lose))
    |> List.maxBy snd
    |> snd