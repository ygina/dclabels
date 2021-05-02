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
    // let p = Priv::new(alice & carla);
    println!("Label 1: {}", l1);
    println!("Label 2: {}", l2);
    // println!("Join of labels: {}", DCLabel::lub(l1, l2));
    // println!("Meet of labels: {}", DCLabel::glb(l1, l2));
    // println!("Privileges: {}", p);
    // println!("Label 1 flows to Label 2? {}", DCLabel::can_flow_to(l1, l2));
    // println!("Label 1 flows to Label 2 given privileges? {}",
    //     DCLabel::can_flow_to_p(l1, l2));

    // Output:
    // Label 1: carla & (alice | bob) % alice & carla
    // Label 2: djon % alice
    // Join of labels: carla & djon & (alice | bob) % alice
    // Meet of labels: (carla | djon) & (alice | bob | djon) % alice & carla
    // Privileges: PrivTCB (alice & carla)
    // Label 1 flows to Label 2? False
    // Label 1 flows to Label 2 given privileges? True
}