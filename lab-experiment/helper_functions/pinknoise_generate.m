%generate 2D pink noise

function dat = pinknoise_generate(num_rows, num_cols)

pink_noise = -1;
dat = spatialPattern([num_rows, num_cols], pink_noise);

%histogram equalization
dat = dat - min(dat(:));
dat = dat / abs(max(dat(:)));