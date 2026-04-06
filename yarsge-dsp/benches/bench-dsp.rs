use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::num::NonZero;
use std::time::Duration;
use yarsge_dsp::Fir;

fn sine(amp: f64, freq: f64, time: f64) -> f32 {
    // assume phase = 0, not much use for it.
    (amp * f64::sin(std::f64::consts::TAU * freq * time)) as f32
}

fn sine_gb(freq: f64, samples: u32) -> Vec<[f32; 2]> {
    let timescale = f64::from(1 << 22).recip();

    (0..samples)
        .map(|it| [sine(0.1, freq, f64::from(it) * timescale); 2])
        .collect()
}

fn criterion_benchmark(c: &mut Criterion) {
    let fir = Fir::new(NonZero::new(375).unwrap(), NonZero::new(32768).unwrap());

    let mut g = c.benchmark_group("fir");
    g.measurement_time(Duration::from_secs(30));
    g.sample_size(5000);

    for size in [100, 1000, 10_000, 50_000] {
        g.throughput(criterion::Throughput::ElementsAndBytes {
            elements: u64::from(size),
            bytes: ((size as usize) * std::mem::size_of::<[f32; 2]>()) as u64,
        });

        let input = sine_gb(440.0, size);

        g.bench_with_input(
            BenchmarkId::new("filter", size),
            &(&fir, input),
            |bench, &(fir, ref i)| {
                bench.iter_batched(
                    || (fir.clone(), i, Vec::new()),
                    |(mut fir, x, mut y)| {
                        fir.filter(x, &mut y);
                    },
                    criterion::BatchSize::PerIteration,
                );
            },
        );
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
