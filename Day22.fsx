#load "Helpers.fsx"

open System

let (hp,dmg) =
    Helpers.Web.getInput 22
    |> Array.map (fun s -> s.Replace("Hit Points: ","")
                            .Replace("Damage: ", "")
    )
    |> Array.map int
    |> (fun [|a1;a2|] -> (a1,a2))

type Me = { Hp : int; Mana : int; Armor : int }
type Boss = { Hp : int; Dmg : int }
type Turn = | Mine | Boss


type EffectEffect = | ArmorInc | DmgInc | ManaInc
type Effect = { TurnsLeft : int; EffectEffect : EffectEffect }

type Spell = { Cost : int; Dmg : int; Heal : int; Effect : Effect option }

let boss = { Hp = hp; Dmg = dmg }
let me = { Hp = 50; Mana = 500; Armor = 0 }

let spells = [
    { Cost = 53; Dmg = 4; Heal = 0; Effect = None }
    { Cost = 73; Dmg = 2; Heal = 2; Effect = None }
    { Cost = 113; Dmg = 0; Heal = 0; Effect = Some { TurnsLeft = 6; EffectEffect = ArmorInc } }
    { Cost = 173; Dmg = 0; Heal = 0; Effect = Some { TurnsLeft = 6; EffectEffect = DmgInc } }
    { Cost = 229; Dmg = 0; Heal = 0; Effect = Some { TurnsLeft = 5; EffectEffect = ManaInc } }
]

let mutable minSpend = Int32.MaxValue
let rec fight me (boss : Boss) currSpend effects turn =
    let effectsInPlay = effects |> List.filter (fun e -> e.TurnsLeft > 0) |> List.map (fun e -> e.EffectEffect) |> Set.ofList
    let effectDmg = if effectsInPlay.Contains(DmgInc) then 3 else 0
    let effectArmor = if effectsInPlay.Contains(ArmorInc) then 7 else 0
    let effectMana  = if effectsInPlay.Contains(ManaInc) then 101 else 0
    let meAfterEffect = { me with Armor = effectArmor; Mana = me.Mana + effectMana }
    let bossAfterEffect = { boss with Hp = boss.Hp - effectDmg }
    if (bossAfterEffect.Hp <= 0 && meAfterEffect.Hp > 0) then
        if (currSpend < minSpend) then
            minSpend <- currSpend
            printfn "New min: %i" minSpend
        else
            ()
    else if (currSpend >= minSpend || meAfterEffect.Hp <= 0) then
        ()
    else
        match turn with
        | Mine ->
            for s in spells
                     |> List.filter (fun s' -> s'.Cost <= meAfterEffect.Mana)
                     |> List.filter (fun s' -> s'.Effect.IsNone || not (effectsInPlay.Contains(s'.Effect.Value.EffectEffect))) do
                let nextMe = { meAfterEffect with Hp = meAfterEffect.Hp + s.Heal; Mana = meAfterEffect.Mana - s.Cost }
                let nextBoss = { bossAfterEffect with Hp = bossAfterEffect.Hp - s.Dmg }
                let nextEffects =
                    effects |> List.map (fun e -> { e with TurnsLeft = e.TurnsLeft - 1 })
                    |> List.append ([s.Effect] |> List.choose id)
                //printfn "Player casts %A" s
                fight nextMe nextBoss (currSpend + s.Cost) nextEffects Boss
        | Boss ->
            let nextMe = { meAfterEffect with Hp = meAfterEffect.Hp - (max 1 (boss.Dmg - meAfterEffect.Armor)) }
            let nextEffects =
                effects
                |> List.map (fun e -> { e with TurnsLeft = e.TurnsLeft - 1 })
            fight nextMe bossAfterEffect currSpend nextEffects Mine

fight me boss 0 [] Mine

(*


For example, suppose the player has 10 hit points and 250 mana, and that the boss has 13 hit points and 8 damage:
*)

fight { Armor = 0; Hp = 10; Mana = 250 } { Hp = 13; Dmg = 8 } 0 [] Mine

// Part 2

let mutable minSpend2 = 1242
let rec fight2 me (boss : Boss) currSpend effects turn =
    let effectsInPlay = effects |> List.filter (fun e -> e.TurnsLeft > 0) |> List.map (fun e -> e.EffectEffect) |> Set.ofList
    let effectDmg = if effectsInPlay.Contains(DmgInc) then 3 else 0
    let effectArmor = if effectsInPlay.Contains(ArmorInc) then 7 else 0
    let effectMana  = if effectsInPlay.Contains(ManaInc) then 101 else 0
    //printfn "Effects: Dmg = %i, Armor = %i, Mana = %i" effectDmg effectArmor effectMana
    let meAfterEffect =
        match turn with
        | Mine -> { me with Armor = effectArmor; Mana = me.Mana + effectMana; Hp = me.Hp - 1 }
        | Boss -> { me with Armor = effectArmor; Mana = me.Mana + effectMana }
    let bossAfterEffect = { boss with Hp = boss.Hp - effectDmg }
    //printfn "Boss: %A" bossAfterEffect
    //printfn "Me: %A" meAfterEffect
    //printfn "CurrSpend: %i" currSpend
    //printfn "Min spend: %i" minSpend2
    //printfn ""
    if (bossAfterEffect.Hp <= 0 && meAfterEffect.Hp > 0) then
        if (currSpend < minSpend2) then
            minSpend2 <- currSpend
            printfn "New min: %i" minSpend2
        else
            ()
    else if (currSpend >= minSpend2 || meAfterEffect.Hp <= 0) then
        ()
    else
        //printfn "here"
        match turn with
        | Mine ->
            //printfn "here2"
            let effectsInPlay2 = effects |> List.filter (fun e -> e.TurnsLeft > 1) |> List.map (fun e -> e.EffectEffect) |> Set.ofList
            for s in spells
                     |> List.filter (fun s' -> s'.Cost <= meAfterEffect.Mana)
                     |> List.filter (fun s' -> s'.Effect.IsNone || not (effectsInPlay2.Contains(s'.Effect.Value.EffectEffect))) do
                //printfn "here3"
                let nextMe = { meAfterEffect with Hp = meAfterEffect.Hp + s.Heal; Mana = meAfterEffect.Mana - s.Cost }
                let nextBoss = { bossAfterEffect with Hp = bossAfterEffect.Hp - s.Dmg }
                let nextEffects =
                    effects |> List.map (fun e -> { e with TurnsLeft = e.TurnsLeft - 1 })
                    |> List.append ([s.Effect] |> List.choose id)
                let nextSpend = (currSpend + s.Cost)
                //printfn "Player casts %A" s
                if (nextBoss.Hp <= 0) then
                    if (nextSpend < minSpend2) then
                        minSpend2 <- nextSpend
                        printfn "New min: %i" minSpend2
                else
                    fight2 nextMe nextBoss nextSpend nextEffects Boss
                
        | Boss ->
            let nextMe = { meAfterEffect with Hp = meAfterEffect.Hp - (max 1 (bossAfterEffect.Dmg - meAfterEffect.Armor)) }
            let nextEffects =
                effects
                |> List.map (fun e -> { e with TurnsLeft = e.TurnsLeft - 1 })
            if (nextMe.Hp <= 0) then
                ()
            else
                fight2 nextMe bossAfterEffect currSpend nextEffects Mine

fight2 me boss 0 [] Mine

// 1242 too high