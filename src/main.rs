#![warn(unused_crate_dependencies)]

use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    str::FromStr,
};

use gloo_timers::callback::Timeout;
use js_sys::Math;
use web_sys::{window, HtmlInputElement, HtmlSelectElement};
use yew::prelude::*;

const MAX_PIPS: usize = 10;
const MP: usize = MAX_PIPS + 1;
const MPT: usize = MP * (MP + 1) / 2;
const DP_SIZE: usize = ((75 - 25) / 10 + 1) * MPT.pow(3);

#[derive(Clone, Copy, PartialEq, Eq)]
struct State {
    chance: u8,
    available: [u8; 3],
    success: [u8; 3],
}
impl State {
    fn key(&self) -> usize {
        let State {
            chance,
            available,
            success,
        } = *self;
        let mut r = usize::from((chance - 25) / 10);
        for i in 0..3 {
            let used = MAX_PIPS - usize::from(available[i]);
            let s = usize::from(success[i]);
            r = r * MPT + used * (used + 1) / 2 + s;
        }
        r
    }
    fn hit(mut self, ix: usize, success: bool) -> Self {
        if success && self.chance > 25 {
            self.chance -= 10;
        }
        if !success && self.chance < 75 {
            self.chance += 10;
        }
        assert!(self.available[ix] > 0);
        self.available[ix] -= 1;
        if success {
            self.success[ix] += 1;
        }
        self
    }
    fn terminal(&self) -> bool {
        self.available == [0; 3]
    }
}

#[test]
fn test_dense_key_space() {
    let mut v = vec![false; DP_SIZE];
    for chance in (25..=75).step_by(10) {
        for a1 in 0..=MAX_PIPS {
            for s1 in 0..=(MAX_PIPS - a1) {
                for a2 in 0..=MAX_PIPS {
                    for s2 in 0..=(MAX_PIPS - a2) {
                        for a3 in 0..=MAX_PIPS {
                            for s3 in 0..=(MAX_PIPS - a3) {
                                let state = State {
                                    chance,
                                    available: [a1 as u8, a2 as u8, a3 as u8],
                                    success: [s1 as u8, s2 as u8, s3 as u8],
                                };
                                assert!(!v[state.key()]); // no overlaps
                                v[state.key()] = true;
                            }
                        }
                    }
                }
            }
        }
    }
    assert!(v == vec![true; DP_SIZE]); // all slots used
}

#[derive(Clone, Copy, PartialEq)]
struct Objective {
    min_1: u8,
    min_2: u8,
    max_3: u8,
    tiebreaking_weight: [f64; 3],
}

impl Objective {
    fn evaluate(&self, state: State) -> (bool, f64) {
        let [s1, s2, s3] = state.success;
        let ok = s1 >= self.min_1 && s2 >= self.min_2 && s3 <= self.max_3;
        let [w1, w2, w3] = self.tiebreaking_weight;
        let weight = f64::from(s1) * w1 + f64::from(s2) * w2 + f64::from(s3) * w3;
        (ok, weight)
    }
}

struct Dp {
    memo: Vec<(f64, f64)>,
}

impl Dp {
    fn init(pips: u8, objective: Objective) -> Dp {
        let mut memo = vec![(-1.0, -1.0); DP_SIZE];
        for chance in (25..=75).step_by(10) {
            for p1 in 0..=pips {
                for p2 in 0..=pips {
                    for p3 in 0..=pips {
                        let st = State {
                            chance,
                            available: [0; 3],
                            success: [p1, p2, p3],
                        };
                        let (ok, weight) = objective.evaluate(st);
                        memo[st.key()] = (if ok { 1.0 } else { 0.0 }, weight);
                    }
                }
            }
        }
        Dp { memo }
    }

    fn evaluate(&mut self, state: State, return_path: bool) -> (f64, f64, usize) {
        let key = state.key();
        if return_path && state.terminal() {
            let (s1, s2) = self.memo[key];
            return (s1, s2, 3);
        }
        if !return_path && self.memo[key].0 >= 0.0 {
            let (s1, s2) = self.memo[key];
            return (s1, s2, 0);
        }
        let chance = f64::from(state.chance) * 0.01;
        let (s1, s2, ix) = (0..3)
            .filter(|&ix| state.available[ix] > 0)
            .map(|ix| {
                let (p1, p2, _) = self.evaluate(state.hit(ix, true), false);
                let (n1, n2, _) = self.evaluate(state.hit(ix, false), false);
                (
                    p1 * chance + n1 * (1.0 - chance),
                    p2 * chance + n2 * (1.0 - chance),
                    ix,
                )
            })
            .max_by(|(a1, a2, aix), (b1, b2, bix)| {
                // Tiebreak by lower effect index for aesthetic reasons
                (a1, a2)
                    .partial_cmp(&(b1, b2))
                    .expect("NaN happened")
                    .then(bix.cmp(aix))
            })
            .unwrap();
        self.memo[key] = (s1, s2);
        (s1, s2, ix)
    }
}

fn select<T: Copy + Display + FromStr + PartialEq + 'static>(
    values: impl Iterator<Item = T>,
    state: UseStateHandle<T>,
    name: &'static str,
) -> Html
where
    <T as FromStr>::Err: Debug,
{
    // work around yewstack/yew#2530
    let select_node_ref = use_node_ref();
    use_effect_with_deps(
        {
            let select_node_ref = select_node_ref.clone();
            move |value: &T| {
                select_node_ref
                    .cast::<HtmlSelectElement>()
                    .expect("select element")
                    .set_value(&value.to_string());
                || {}
            }
        },
        *state,
    );
    let onchange = {
        let state = state.clone();
        move |e: Event| {
            state.set(
                e.target_unchecked_into::<HtmlSelectElement>()
                    .value()
                    .parse()
                    .expect("value should be integer"),
            );
        }
    };
    html! {
        <select name={name} onchange={onchange} ref={select_node_ref}>
            {values.map(|c| html! {
                <option value={c.to_string()}>{c}</option>
            }).collect::<Html>()}
        </select>
    }
}

fn number<T: Display + FromStr + PartialEq + 'static>(
    state: UseStateHandle<T>,
    name: &'static str,
    min: &'static str,
    max: &'static str,
    step: &'static str,
) -> Html {
    let onchange = {
        let state = state.clone();
        move |e: Event| {
            // allow the user to temporarily input garbage,
            // but it will be overwritten on the next render
            if let Ok(v) = e
                .target_unchecked_into::<HtmlInputElement>()
                .value()
                .parse()
            {
                state.set(v)
            }
        }
    };
    html! {
        <input name={name} type="number" min={min} max={max} step={step} onchange={onchange} value={state.to_string()} />
    }
}

#[function_component(Model)]
fn model() -> Html {
    let chance = use_state_eq(|| 75);
    let pips = use_state_eq(|| 10);
    let min1 = use_state_eq(|| 7);
    let min2 = use_state_eq(|| 7);
    let max3 = use_state_eq(|| 4);
    let w1 = use_state_eq(|| 1.0);
    let w2 = use_state_eq(|| 1.0);
    let w3 = use_state_eq(|| -1.0);
    let s1 = use_state_eq(|| vec![]);
    let s2 = use_state_eq(|| vec![]);
    let s3 = use_state_eq(|| vec![]);
    let autobrick_enabled =
        use_state_eq(|| Some(window()?.location().search().ok()?.contains("autobrick")));
    let autobrick_enabled = *autobrick_enabled == Some(true);
    let autobrick = use_state_eq(|| None);
    let do_hit = |s: &UseStateHandle<Vec<bool>>, chance: &UseStateHandle<i32>, success: bool| {
        let mut next = (**s).clone();
        next.push(success);
        s.set(next);
        if success && **chance > 25 {
            chance.set(**chance - 10);
        }
        if !success && **chance < 75 {
            chance.set(**chance + 10);
        }
    };
    let ctrl = |s: UseStateHandle<Vec<bool>>, id: &'static str, should_hit: bool| {
        html! {
            <div id={id} class={if should_hit { "status should_hit" } else { "status" }}>
                {
                    (0..*pips).map(|ix| match s.get(ix as usize) {
                        Some(true) => html! { <span class="pip success">{"+1"}</span> },
                        Some(false) => html! { <span class="pip fail">{"0"}</span> },
                        None => html! { <span class="pip empty">{"-"}</span> },
                    }).collect::<Html>()
                }
                {
                    [("+1", true), ("0", false)].iter().map(|&(label, success)| html! {
                        <button disabled={s.len() >= *pips} class={"hit"} onclick={
                            let s = s.clone();
                            let chance = chance.clone();
                            move |_e: MouseEvent| {
                                do_hit(&s, &chance, success);
                            }
                        }>{label}</button>
                    }).collect::<Html>()
                }
                <button disabled={s.len() == 0} class={"hit"} onclick={
                    let s = s.clone();
                    move |_e: MouseEvent| {
                        let mut next = (*s).clone();
                        next.pop();
                        s.set(next);
                    }
                }>{"Unhit"}</button>
                <span class="sum">
                {
                    format!("{}", s.iter().filter(|&&x| x).count())
                }
                </span>
            </div>
        }
    };
    // Do some jank caching since this calculation is all happening on the main thread
    let dp_args = (
        *pips as u8,
        Objective {
            min_1: *min1,
            min_2: *min2,
            max_3: *max3,
            tiebreaking_weight: [*w1, *w2, *w3],
        },
    );
    let dp_state = use_state(|| RefCell::new(None));
    let mut dp_cell = dp_state.borrow_mut();
    let dp = match *dp_cell {
        Some((ref args, ref mut dp)) if *args == dp_args => dp,
        _ => &mut dp_cell.insert((dp_args, Dp::init(dp_args.0, dp_args.1))).1,
    };

    let states = [&*s1, &*s2, &*s3];
    let state = State {
        chance: *chance as u8,
        available: states.map(|s| (*pips).saturating_sub(s.len()) as u8),
        success: states.map(|s| s.iter().filter(|&&x| x).count() as u8),
    };
    let (success_rate, expected_weight, which_hit) = dp.evaluate(state, true);
    let analysis = html! {
        <div id="analysis">
            {if state.terminal() { "".to_string() } else { format!("Next hit: effect {}", which_hit + 1) }}
            <br />
            {format!("Success probability: {:.3}%", success_rate * 100.0)}
            <br />
            {format!("Expected tiebreaking weight: {}", expected_weight)}
        </div>
    };
    use_effect({
        let (s1, s2, s3) = (s1.clone(), s2.clone(), s3.clone());
        let (autobrick, pips, chance) = (autobrick.clone(), pips.clone(), chance.clone());
        let target = *autobrick;
        let current = s1.len() + s2.len() + s3.len();
        move || {
            log::info!("{:?}", *autobrick);
            let timeout = match *autobrick {
                Some(v) if v < *pips * 3 => {
                    let autobrick = autobrick.clone();
                    Some(Timeout::new(100, move || {
                        autobrick.set(Some(v + 1));
                    }))
                }
                Some(_) => {
                    autobrick.set(None);
                    None
                }
                None => None,
            };
            if target.map_or(false, |t| t > current) {
                let success = Math::random() * 100.0 < f64::from(*chance);
                let s = [&s1, &s2, &s3][which_hit];
                do_hit(s, &chance, success);
            }
            move || {
                if let Some(t) = timeout {
                    t.cancel();
                }
            }
        }
    });
    let autobrick_button = if autobrick_enabled {
        let (autobrick, s1, s2, s3, chance) = (
            autobrick.clone(),
            s1.clone(),
            s2.clone(),
            s3.clone(),
            chance.clone(),
        );
        let onclick = move |_e: MouseEvent| {
            autobrick.set(None);
            s1.set(vec![]);
            s2.set(vec![]);
            s3.set(vec![]);
            chance.set(75);
            autobrick.set(Some(0));
        };
        html! {
            <button onclick={onclick}>{"Autobrick"}</button>
        }
    } else {
        html! {
            <></>
        }
    };
    return html! {
        <>
            <div id="inputs">
                <div>
                    <label for="pips">{"Pips"}</label>
                    {select(4..=10, pips.clone(), "pips")}
                </div>
                <div>
                    <label for="min1">{"Effect 1 min"}</label>
                    {number(min1, "min1", "0", "10", "1")}
                    <label for="w1">{"weight"}</label>
                    {number(w1, "w1", "", "", "0.1")}
                </div>
                <div>
                    <label for="min2">{"Effect 2 min"}</label>
                    {number(min2, "min2", "0", "10", "1")}
                    <label for="w2">{"weight"}</label>
                    {number(w2, "w2", "", "", "0.1")}
                </div>
                <div>
                    <label for="max3">{"Effect 3 max"}</label>
                    {number(max3, "max3", "0", "10", "1")}
                    <label for="w3">{"weight"}</label>
                    {number(w3, "w3", "", "", "0.1")}
                </div>
                <div>
                    <label for="chance">{"Chance of success"}</label>
                    {select((25..=75).step_by(10), chance.clone(), "chance")}
                </div>
            </div>
            <div id="hits">
                {"Current status"}
                {ctrl(s1.clone(), "status1", which_hit == 0)}
                {ctrl(s2.clone(), "status2", which_hit == 1)}
                {ctrl(s3.clone(), "status3", which_hit == 2)}
                <button onclick={
                    let (s1, s2, s3) = (s1.clone(), s2.clone(), s3.clone());
                    let (chance, autobrick) = (chance.clone(), autobrick.clone());
                    move |_e: MouseEvent| {
                        autobrick.set(None);
                        s1.set(vec![]);
                        s2.set(vec![]);
                        s3.set(vec![]);
                        chance.set(75);
                    }
                }>{"Reset"}</button>
                {autobrick_button}
            </div>
            {analysis}
        </>
    };
}

fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::start_app::<Model>();
}
