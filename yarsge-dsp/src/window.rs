pub fn hann(len: usize) -> Vec<f64> {
    assert!(len >= 2);

    // the window is symmetric so we can just mirror the second half from the first half
    let half_len = len.div_ceil(2);

    let scale = std::f64::consts::PI / ((len - 1) as f64);

    let mut window = vec![0.0; len];

    for (idx, elem) in window[..half_len].iter_mut().enumerate() {
        *elem = (scale * (idx as f64)).sin().powi(2);
    }

    let (orig, mirror) = window.split_at_mut(half_len);

    for (mirror, orig) in mirror.iter_mut().rev().zip(orig) {
        *mirror = *orig;
    }

    window
}
