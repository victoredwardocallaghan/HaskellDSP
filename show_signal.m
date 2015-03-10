%% [] = show_signal('testsig.bin');
function [] = show_signal (filename)
    sig = load_sc16q11(filename);
    sig = sig ./ 2048;
    n = size(sig);              % Number of bits to process
    
    pkg load signal;
    pkg load communications;
    
    [b, a] = cheby2( 4, 80, 0.001, 'high' );
    filtered_samples = filter(b,a,sig);
    Complex_FFT = fft( filtered_samples );

    plot3(sig);
            grid on;
            xlabel('Time');
            ylabel('In phase (I)');
            zlabel('Quadrature (Q)');

    figure; plot(sig);
            title('IQ Constellation Diagram');
            grid on;
            xlabel('In phase (I)');
            ylabel('Quadrature (Q)');

    figure; plot( 1:n , Complex_FFT );
            title('Complex FFT');
            grid on;
            xlabel('Frequency');
            ylabel('Power');

    figure;  plot( real(sig), 'color', 'red'  );
    hold on; plot( imag(sig), 'color', 'blue' );
             grid on;
             xlabel('Time');
             ylabel('Amplitude');
             legend({'In phase (I)', 'Quadrature (Q)'});
