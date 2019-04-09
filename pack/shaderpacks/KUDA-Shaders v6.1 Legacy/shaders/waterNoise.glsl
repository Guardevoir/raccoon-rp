#extension GL_EXT_gpu_shader4 : enable

//This file uses functions made by Zombye. They will eventually be replaced.

const int noiseTextureResolution     = 96;   

float square (float x) { return x * x; }
vec2  square (vec2  x) { return x * x; }
vec3  square (vec3  x) { return x * x; }
vec4  square (vec4  x) { return x * x; }

#define WAVE_SCALE 0.00084
#define WAVE_SCALE2 0.0044
#define WAVE_SPEED 0.000047
#define WAVE_SPEED2 0.000175

float cubicHermite01(float x) { return square(x) * (3 - (2 * x)); } // AKA cubesmooth in Ebin, and CubicSmooth in SEUS.
vec2  cubicHermite01(vec2  x) { return square(x) * (3 - (2 * x)); } // Applies an S curve in the domain (0 - 1), with 0 = 0 and 1 = 1
vec3  cubicHermite01(vec3  x) { return square(x) * (3 - (2 * x)); }
vec4  cubicHermite01(vec4  x) { return square(x) * (3 - (2 * x)); }

vec2 pixelCenterBias(
	in vec2 coord,
	in vec2 resolution
) {
	// Biases the texture coordinates to the pixel center, makes bilinear filtering look much smoother.

	coord *= resolution;
	coord = floor(coord) + (cubicHermite01(fract(coord)) + 0.5);
	return coord / resolution;
}

float getWaves(in vec3 position)
{
	const uint numWaves = 4u;
	float waveTime = frameTimeCounter * WAVE_SPEED*1.4;
	float waveTime2 = frameTimeCounter * WAVE_SPEED*4.4;
	float waveTime3 = frameTimeCounter * WAVE_SPEED*7.4;

	// Base translation
	vec2 p = -(position.xz + position.y) + waveTime;
	vec2 p2 = -(position.zx + position.y) - waveTime2;
	vec2 p3 = -(position.xz + position.y) + waveTime3;

	// Scale
	p /= WAVE_SCALE*1.05;
	p2 /= WAVE_SCALE*1.2;
	p3 /= WAVE_SCALE*1.05;

	const float weightArray[numWaves] = float[numWaves] (
		1.0,
		8.0,
		15.0,
		25.0
	);

	vec2 pArray[numWaves] = vec2[numWaves] (
		(p / 1.6) + waveTime * vec2(0.03, 0.07),
		(p2 / 3.1) + waveTime * vec2(0.08, 0.06),
		(p / 4.7) + waveTime * vec2(0.07, 0.10),
		(p3 / 8.9) + waveTime * vec2(0.04, 0.02)
	);

	const vec2 scaleArray[numWaves] = vec2[numWaves] (
		vec2(2.0, 1.4),
		vec2(1.7, 0.7),
		vec2(1.0, 1.2),
		vec2(1.0, 0.8)
	);

	vec2 translationArray[numWaves] = vec2[numWaves] (
		vec2(pArray[0].x * 0.0, pArray[0].y * 0.0),
		vec2(pArray[1].y * 0.0, pArray[1].x * 0.0),
		vec2(pArray[2].x * 0.0, pArray[2].y * 0.0),
		vec2(pArray[3].y * 0.0, pArray[3].x * 0.0)
	);

    float waves   = 0.0;
    float weights = 0.0;

	for(uint id = 0u; id < numWaves; id++)
	{
        float wave = texture2D(noisetex, pixelCenterBias((pArray[id] * scaleArray[id]) + translationArray[id], vec2(1024.0))).r;
		waves   += wave * weightArray[id];
		weights += weightArray[id];
	}

	waves /= weights;

	waves *= 0.06;
	waves -= 0.06;

	return waves;
}