%% [sig, samples_i, samples_q] = load_sc16q11(filename)
function [signal, samples_i, samples_q] = load_sc16q11 (filename)
    if nargin != 1
        error("Usage: [sig, samples_i, samples_q] = load_sc16q11(filename)")
    endif

    f = fopen(filename, "r", "ieee-le");
    if f < 0
        signal = [];
        samples_i = []
        samples_q = []
        error("Failed to open specified file");
    endif

    samples = fread(f, Inf, "int16", "ieee-le");

    endd = floor(rows(samples) / 2 ) * 2;

    samples_i = samples(1:2:endd, :);
    samples_q = samples(2:2:endd, :);

    signal = samples_i + j * samples_q;

    fclose(f);
endfunction
