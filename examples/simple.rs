use dclabels::*;

fn main() {
    // Simple secrecy component example
    let alice : CNF = "alice".into();
    let bob : CNF   = "bob".into();
    let carla : CNF = "carla".into();
    let djon : CNF  = "djon".into();

    // TODO: find a way around these clones
    let s: CNF = (alice.clone() | bob.clone()) & carla.clone();
    // Simple integrity component example
    let i: CNF = alice.clone() & carla.clone();

    // Simple label
    let l1: DCLabel = s % i;
    // Simple label
    let l2 = djon.clone() % alice.clone();

    // Creating privilege using constructor from TCB
    let p: Priv = alice.clone() & carla.clone();
    // Label 1: carla /\ (alice \/ bob) %% alice /\ carla
    println!("Label 1: {}", l1);
    // Label 2: djon %% alice
    println!("Label 2: {}", l2);
    // Join of labels: carla /\ djon /\ (alice \/ bob) %% alice
    println!("Join of labels: {}", l1.clone().lub(l2.clone()));
    // Meet of labels: (carla \/ djon) /\ (alice \/ bob \/ djon) %% alice /\ carla
    println!("Meet of labels: {}", l1.clone().glb(l2.clone()));
    // Privileges: (alice /\ carla)
    println!("Privileges: {}", p);
    // Label 1 flows to Label 2? false
    println!("Label 1 flows to Label 2? {}", l1.can_flow_to(&l2));
    // Label 1 flows to Label 2 given privileges? true
    println!("Label 1 flows to Label 2 given privileges? {}",
        l1.can_flow_to_p(&l2, &p));
}