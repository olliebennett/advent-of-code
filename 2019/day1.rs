fn main() {
  println!("Advent of Code - Day 1");

  let input = data();
  let fuel_sum: i32 = input.into_iter().map(|f| fuel_for_mass(*f)).sum();

  println!("Part 1: Fuel for modules: {}", fuel_sum);

  let fuel_with_fuel_sum: i32 = input.into_iter().map(|f| fuel_for_mass_plus_fuel(*f)).sum();

  println!("Part 2: Fuel for modules plus fuel for that fuel: {}", fuel_with_fuel_sum);
}

fn fuel_for_mass_plus_fuel(mass: i32) -> i32 {
  let mut fuel = fuel_for_mass(mass);
  let mut extra_fuel = fuel_for_mass(fuel);
  while extra_fuel > 0 {
    fuel = fuel + extra_fuel;
    extra_fuel = fuel_for_mass(extra_fuel);
  }

  return fuel;
}

fn fuel_for_mass(mass: i32) -> i32 {
  // to find fuel required, take its mass, divide by three, round down, and subtract 2.
  return mass / 3 - 2;
}

fn data()-> [i32; 100] {
  return [
    101005,
    139223,
    112833,
    70247,
    131775,
    106730,
    118388,
    138683,
    80439,
    71060,
    120862,
    67201,
    70617,
    79783,
    114813,
    77907,
    78814,
    107515,
    113507,
    81865,
    88130,
    75120,
    66588,
    56023,
    98080,
    128472,
    96031,
    118960,
    54069,
    112000,
    62979,
    105518,
    73342,
    52270,
    128841,
    68267,
    70789,
    94792,
    100738,
    102331,
    83082,
    77124,
    97360,
    86165,
    66120,
    139042,
    50390,
    105308,
    94607,
    58225,
    77894,
    118906,
    127277,
    101446,
    58897,
    93876,
    53312,
    117154,
    77448,
    62041,
    99069,
    87375,
    134854,
    108561,
    126406,
    53809,
    90760,
    121650,
    79573,
    134734,
    148021,
    84263,
    54390,
    132706,
    148794,
    67302,
    146885,
    76108,
    76270,
    54548,
    146920,
    145282,
    129509,
    144139,
    141713,
    62547,
    149898,
    96746,
    83583,
    107758,
    63912,
    142036,
    112281,
    91775,
    75809,
    82250,
    144667,
    140140,
    98276,
    103479
  ]
}

#[test]
fn test_fuel_for_mass() {
  assert_eq!(fuel_for_mass(12), 2);
  assert_eq!(fuel_for_mass(14), 2);
  assert_eq!(fuel_for_mass(1969), 654);
  assert_eq!(fuel_for_mass(100756), 33583);
}

#[test]
fn test_fuel_for_mass_plus_fuel() {
  // total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966
  assert_eq!(fuel_for_mass_plus_fuel(1969), 966);
  assert_eq!(fuel_for_mass_plus_fuel(100756), 50346);
}
