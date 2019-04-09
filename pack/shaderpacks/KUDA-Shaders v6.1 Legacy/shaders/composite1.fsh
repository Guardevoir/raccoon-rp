#version 120
#extension GL_ARB_shader_texture_lod : enable

/*



			███████ ███████ ███████ ███████ █
			█          █    █     █ █     █ █
			███████    █    █     █ ███████ █
			      █    █    █     █ █
			███████    █    ███████ █       █

	Before you change anything here, please keep in mind that
	you are allowed to modify my shaderpack ONLY for yourself!

	Please read my agreement for more informations!
		- http://dedelner.net/agreement/



*/

#define reflections
#define rainPuddles
#define waterShader
#define waterRefraction
	//#define RGB_Offset
#define clouds
#define stars
#define airDensity 1.0 // [0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0]
#define windSpeed 1.0 // [0.1 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6]
//#define dynamicWeather
//#define useFixedWeatherRatio
#define volumetricLight
	#define vlQuality 0.5 // [0.3 0.5 0.75 1.0 1.5 2.0]
	#define vlRaysStrength 0.5 // [0.5 0.6 0.7 0.8 0.9 1.0]

//#define animateUsingWorldTime

#define maxColorRange 20.0

#define upVector gbufferModelView[1].xyz

varying vec3 sunVector;
varying vec3 moonVector;
varying vec3 lightVector;

varying vec4 timeVector;

varying vec2 texcoord;
varying float weatherRatio;

varying vec3 rayColor;
varying vec3 sunColor;
varying vec3 moonColor;
varying vec3 skyColor;
varying vec3 horizonColor;
varying vec3 fogColor;
varying vec3 underwaterColor;
varying vec3 cloudColor;

varying float TimeSunrise;
varying float TimeNoon;
varying float TimeSunset;
varying float TimeMidnight;
varying float TimeDay;
varying float DayToNightFading;

uniform sampler2D gcolor;
uniform sampler2D gnormal;
uniform sampler2D gdepth;
uniform sampler2D composite;
uniform sampler2D gaux1;
uniform sampler2D gaux2;
uniform sampler2D gaux3;
uniform sampler2D gaux4;
uniform sampler2D depthtex0;
uniform sampler2D depthtex1;
uniform sampler2D noisetex;

uniform mat4 gbufferProjection;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelViewInverse;
uniform mat4 shadowModelView;
uniform mat4 shadowProjection;
uniform mat4 shadowModelViewInverse;
uniform mat4 shadowProjectionInverse;
uniform mat4 gbufferModelView;


uniform vec3 cameraPosition;
uniform vec3 sunPosition;
uniform vec3 moonPosition;
uniform vec3 upPosition;

uniform ivec2 eyeBrightnessSmooth;
uniform ivec2 eyeBrightness;

uniform float near;
uniform float far;
uniform float rainStrength;
uniform float frameTimeCounter;
uniform float wetness;
uniform float viewWidth;
uniform float viewHeight;

uniform int worldTime;
uniform int isEyeInWater;

#ifdef animateUsingWorldTime
	#define frameTimeCounter worldTime * 0.0416
#endif

vec3  normal = texture2D(gnormal, texcoord.st).rgb * 2.0 - 1.0;
vec3  gaux2normal = texture2D(gaux2, texcoord.st).rgb * 2.0 - 1.0;
float depth0 = texture2D(depthtex0, texcoord.st).x;
float depth1 = texture2D(depthtex1, texcoord.st).x;
float skyLightmap = clamp(texture2D(gdepth, texcoord.st).r, 0.0, 1.0);
float shading2 = texture2D(gcolor, texcoord.st).a;
float material = texture2D(gdepth, texcoord.st).b;

float gaux3SkyLightmap = clamp(texture2D(gaux3, texcoord.st).r, 0.0, 1.0);
float gaux3Material = texture2D(gaux3, texcoord.st).b;



// Booleans..
float	comp = 1.0 - near / far / far;

bool land	= depth1 < comp;
bool sky	= depth1 > comp;

bool raindrops = gaux3Material > 0.69 && gaux3Material < 0.71;
bool water = gaux3Material > 0.09 && gaux3Material < 0.11;
bool ice = gaux3Material > 0.19 && gaux3Material < 0.21;
bool stainedGlass = gaux3Material > 0.29 && gaux3Material < 0.31;
bool hand = gaux3Material > 0.49 && gaux3Material < 0.51;
bool GAUX1 = gaux3Material > 0.0;		// Ask for all materials which are stored in gaux3 for gaux1.

bool reflectiveBlocks = water || ice || stainedGlass;



// Common variables..
float globalLightmap = reflectiveBlocks || hand? gaux3SkyLightmap : skyLightmap;

// r = default reflection
// g = wetness reflection
// b = rain puddles
// a = wetness map
vec4 specular = texture2D(gaux4, texcoord.st);

float ambInfluence = 1.0 / airDensity;
float eyeAdapt = eyeBrightnessSmooth.y / 240.0;

const float pi = 3.14159265359;
const float tau = pi*2.0;

// User defined functions..
float linearDepth(float depth){
	return 2.0 * (near * far) / (far + near - (depth) * (far - near));
}

float cdist(vec2 coord) {
	return max(abs(coord.s - 0.5), abs(coord.t - 0.5)) * 2.0;
}

vec3 cameraSpaceToScreenSpace(vec3 fragpos) {

	vec4 pos  = gbufferProjection * vec4(fragpos, 1.0);
			 pos /= pos.w;

	return pos.xyz * 0.5 + 0.5;

}

vec3 cameraSpaceToWorldSpace(vec3 fragpos) {

	vec4 pos  = gbufferProjectionInverse * vec4(fragpos, 1.0);
			 pos /= pos.w;

	return pos.xyz;

}

float luma(vec3 clr) {
	return dot(clr, vec3(0.3333));
}

float rand(vec2 co) {

    float a = 12.9898;
    float b = 78.233;
    float c = 43758.5453;

    float dt= dot(co.xy, vec2(a, b));
    float sn= mod(dt, pi);

    return fract(sin(sn) * c);
}

float autoExpsoure() {

	const float exposureAmount = 1.5;

	float aE_lightmap	= 1.0 - eyeAdapt;
				aE_lightmap = mix(aE_lightmap, 1.0, pow(TimeMidnight, 2.5));

	return 1.0 + aE_lightmap * exposureAmount;

}

float getWorldHorizonPos(vec3 fragpos) {

	float position		= dot(normalize(fragpos.xyz + vec3(0.0, 15.0, 0.0)), upPosition);
	float horizonPos	= mix(pow(clamp(1.0 - pow(abs(position) * 0.05, 1.5), 0.0, 1.0), 5.0), 1.0, 1.0 - clamp(position + length(position), 0.0, 1.0));

	return horizonPos;

}

vec3 drawSky(vec3 fragpos) {

	float horizonStrength = 1.3;

	// Get position.
	float position = dot(normalize(fragpos.xyz), upPosition);
	float sunVector = max(dot(normalize(fragpos), normalize(sunPosition)), 0.0);

	float sun = pow(sunVector, 1.5) * (1.0 - TimeMidnight);

	float horizonPos 	= pow(max(1.0 - abs(position) * 0.012 * ambInfluence, 0.0), 2.8);
	float skyPos		= max(1.0 - abs(position) * 0.00525 * ambInfluence * 0.8, 0.0);

	vec3 clr = mix(skyColor * skyPos, mix(horizonColor, sunColor, sun * 0.7), horizonPos * horizonStrength);
		 	 clr = mix(clr, horizonColor, getWorldHorizonPos(fragpos.xyz));

	return clr;

}

vec3 drawStars(vec3 clr, vec3 fragpos) {

	#ifdef stars

		const float starsScale = 0.05;
		const float starsMovementSpeed = 0.001;

		vec4 worldPos = gbufferModelViewInverse * vec4(fragpos.xyz, 1.0);

		float position = dot(normalize(fragpos.xyz), upPosition);
		float horizonPos = max(1.0 - pow(abs(position) * 0.013, 1.0), 0.0);

		vec2 coord = (worldPos.xz / (worldPos.y / pow(position, 0.75)) * starsScale) + vec2(frameTimeCounter * starsMovementSpeed);

		float noise  = texture2D(noisetex, coord).x;
					noise += texture2D(noisetex, coord * 2.0).x / 2.0;
					noise += texture2D(noisetex, coord * 6.0).x / 6.0;

		noise = max(noise - 1.4, 0.0);
		noise = mix(noise, 0.0, clamp(getWorldHorizonPos(fragpos) + horizonPos, 0.0, 1.0));

		clr = mix(clr, vec3(2.5), noise * TimeMidnight * (1.0 - weatherRatio));

	#endif

	return clr;

}

vec3 drawSun(vec3 clr, vec3 fragpos, bool forReflections) {

	const float sunStrength = 5.0;
	const float moonStrength = 0.8;

	// Get position.
	float sunVector = max(dot(normalize(fragpos), normalize(sunPosition)), 0.0);
	float moonVector = max(dot(normalize(fragpos), normalize(moonPosition)), 0.0);
	float moonVector2 = max(dot(normalize(fragpos), normalize(vec3(moonPosition.x + 3.0, moonPosition.y + 1.0, moonPosition.z + 0.5))), 0.0);

	// Calculate light vectors.
	float sun	= pow(sunVector, 17.0);
	float moon = clamp(pow(moonVector, 10000.0) * 15.0, 0.0, 1.0) * moonStrength;
	float moon2 = clamp(pow(moonVector2, 500.0) * 1.5, 0.0, 1.0);

	float newSun = smoothstep(0.99, 1.0, sun) * sunStrength;

	newSun	 = mix(newSun, 0.0, getWorldHorizonPos(fragpos.xyz));
	moon = mix(moon, 0.0, getWorldHorizonPos(fragpos.xyz));
	moon = mix(moon, 0.0, moon2);

	if (forReflections)	{
		newSun *= shading2;
		moon *= shading2;
	}

	clr = mix(clr, sunColor, newSun * (1.0 - weatherRatio));
	clr = mix(clr, moonColor, moon * (1.0 - weatherRatio));

	return clr;

}

vec3 draw2DClouds(vec3 clr, vec3 fragpos) {

	const float cloudWindSpeed = 0.02;
	const float distortionStrength = 3.0;

	float cloudCover = 0.65;
	float cloudOpacity = 3.0;
	float sunStrength = 4.0;
	float moonStrength = 0.1;
	float shadingStrength = 0.12;

	#ifdef clouds

		// Get position.
		vec4 worldPos = gbufferModelViewInverse * vec4(fragpos, 1.0);
		vec4 worldPos2 = gbufferModelViewInverse * vec4(fragpos + lightVector * 20.0, 1.0);

		float position = dot(normalize(fragpos.xyz), upPosition);
		float horizonPos = max(1.0 - abs(position) * 0.03, 0.0);

		float horizonBorder = min((1.0 - clamp(position + length(position), 0.0, 1.0)) + horizonPos, 1.0);

		float sunVector = max(dot(normalize(fragpos), normalize(sunPosition)), 0.0);
		float moonVector = max(dot(normalize(fragpos), normalize(moonPosition)), 0.0);

		float curvedPos = pow(position, 0.5);


		// Calculate light vectors.
		float sun	= pow(sunVector, 5.0);
		float moon	= pow(moonVector, 1.0);

		vec2 wind = vec2(frameTimeCounter * 0.008) * cloudWindSpeed * windSpeed;

		// Apply distortion.
		//worldPos.x += sin((worldPos.z / worldPos.y) * curvedPos * 0.75) * 2.5 * distortionStrength;
		//worldPos.z += cos((worldPos.x / worldPos.y) * curvedPos * 1.5) * 1.25 * distortionStrength;

		mat2 rot = mat2(cos(0.5), sin(0.5), -sin(0.5), cos(0.50));

		vec2 coord  = ((worldPos.xz / worldPos.y) * curvedPos * 0.0025) * rot + wind * 2.0;
		vec2 coord2  = ((worldPos2.xz / worldPos2.y) * curvedPos * 0.0025) * rot + wind * 2.0;

		float noise  = texture2D(noisetex, coord - wind).x;
					noise += texture2D(noisetex, coord * 4.0 - wind).x / 4.0;
					noise += texture2D(noisetex, coord * 12.0 - wind).x / 12.0;
					noise += texture2D(noisetex, coord * 34.0 - wind).x / 34.0;

		float noise2  = texture2D(noisetex, coord2 - wind).x;
					noise2 += texture2D(noisetex, coord2 * 4.0 - wind).x / 4.0;
					noise2 += texture2D(noisetex, coord2 * 12.0 - wind).x / 12.0;
					//noise2 += texture2D(noisetex, coord2 * 34.0 - wind).x / 34.0;

		cloudCover = mix(cloudCover, 0.0, horizonBorder);
		cloudCover = mix(cloudCover, 0.0, sqrt(weatherRatio));

		#if defined dynamicWeather || defined useFixedWeatherRatio
			cloudCover = mix(1.0, 0.0, sqrt(weatherRatio));
		#endif

		float cloud 					= max(noise - cloudCover * 1.25, 0.0);
		float cloudShading		= max(noise2 - cloudCover * 0.8, 0.0);

		// Apply conditions.
		sunStrength *= mix(1.0, 0.08, TimeMidnight);
		sunStrength *= mix(1.0, 0.1, weatherRatio);
		moonStrength *= mix(1.0, 0.25, TimeDay);
		moonStrength *= mix(1.0, 0.0, weatherRatio);
		shadingStrength *= mix(1.0, 1.4, weatherRatio) * DayToNightFading;

		cloudOpacity = mix(cloudOpacity, cloudOpacity * 2.5, cloudCover);

		vec3 cloudClr = cloudColor;
				 cloudClr = mix(cloudClr, sunColor, sun * sunStrength * cloud * TimeDay);
				 cloudClr = mix(cloudClr, moonColor, moon * moonStrength * cloud * TimeMidnight);

				 cloudClr *= mix(0.75, 1.0, DayToNightFading);
				 cloud *= mix(0.5, 1.0, DayToNightFading);

				 // Shading..
				 cloudClr = mix(cloudClr, skyColor * 0.6, cloudShading * shadingStrength * cloudOpacity);

		clr = mix(clr, cloudClr, min(cloud * cloudOpacity, 1.0) * (1.0 - horizonBorder));

	#endif

	return clr;

}

float getVolumetricRays(vec3 fragpos) {

	vec3 pos = cameraSpaceToScreenSpace(fragpos);

	float vlMipmapping = 1.0 / vlQuality;

	float vlRays  = texture2DLod(composite, pos.st, vlMipmapping <= 1.34? 0.0 : vlMipmapping * 1.1).a;	// If the VL quality is greater than 1.0, enable mipmap.
				vlRays *= DayToNightFading * (1.0 - weatherRatio);

	return vlRays;

}
/*
float getnoise(vec2 pos){
    return fract(sin(dot(pos, vec2(12.9898, 4.1414))) * 43758.5453);
}

float vcnoise(vec3 pos){
    vec3 flr = floor(pos);
    vec3 frc = fract(pos);
    float yadd = 16.0;
    frc = frc*frc*(3.0-2.0*frc);
    
    
    //float noise = getnoise(flr.xz+flr.y*yadd);
    //return noise
    
    
    float noisebdl = getnoise(flr.xz+flr.y*yadd);
    float noisebdr = getnoise(flr.xz+flr.y*yadd+vec2(1.0,0.0));
    float noisebul = getnoise(flr.xz+flr.y*yadd+vec2(0.0,1.0));
    float noisebur = getnoise(flr.xz+flr.y*yadd+vec2(1.0,1.0));
    float noisetdl = getnoise(flr.xz+flr.y*yadd+yadd);
    float noisetdr = getnoise(flr.xz+flr.y*yadd+yadd+vec2(1.0,0.0));
    float noisetul = getnoise(flr.xz+flr.y*yadd+yadd+vec2(0.0,1.0));
    float noisetur = getnoise(flr.xz+flr.y*yadd+yadd+vec2(1.0,1.0));
    float noise= mix(mix(mix(noisebdl,noisebdr,frc.x),mix(noisebul,noisebur,frc.x),frc.z),mix(mix(noisetdl,noisetdr,frc.x),mix(noisetul,noisetur,frc.x),frc.z),frc.y);
    return noise;
}

float groundFog(vec3 worldPos) {
	worldPos.y -= 85.0;
	float density = vcnoise(worldPos / vec3(12.0, 12.0, 12.0));
	density *= exp(-worldPos.y / 8.0);
	density = clamp(density, 0.0, 5.0);
	return density * 1.0;
}
*/
vec3 drawFog(vec3 clr, vec3 fragpos, bool forReflections) {

	const float fogDistance	= 0.005;	// Higher -> near.
	const float lightScatteringAmount	= 0.6;
	float fogDensity = 1.0;

	// Apply conditions.
	fogDensity *= airDensity;
	fogDensity *= mix(1.0, 3.5, rainStrength);
	fogDensity *= mix(1.0, 2.0, TimeMidnight);

	vec3 pos = cameraSpaceToScreenSpace(fragpos);

	float vlRays = getVolumetricRays(fragpos);

	if (bool(isEyeInWater) && !water) vlRays = 0.0;

	//if (!water) vlRays *= 1.0 - texture2D(gaux1, pos.xy).a;

	float fogFactor = 1.0 - exp(-pow(length(fragpos.xyz) * fogDistance, 1.5));

	// Remove fog when player is underwater.
	if (bool(isEyeInWater)) fogFactor = 0.0;

	fogFactor *= eyeAdapt;

	bool fLand = texture2D(depthtex1, pos.xy).x < comp;

	// Do atmospheric scattering.
	float lightVec = max(dot(normalize(fragpos), normalize(lightVector)), 0.0);
	float lightFactor	= pow(lightVec, 5.0) * lightScatteringAmount * mix(1.0, vlRays, float(fLand)) * (1.0 - weatherRatio);
				lightFactor = mix(lightFactor, pow(lightVec, 5.0) * lightScatteringAmount * 0.25, weatherRatio);

	float infl = mix(airDensity, 1.0, TimeMidnight);

	// Air density influence.
	vec3 nFogColor = pow(fogColor * mix(0.5, 1.0, DayToNightFading * (1.0 - weatherRatio)), vec3(1.0 / infl)) / pow(infl, 0.4);
	vec3 nRayClr = pow(rayColor, vec3(ambInfluence));

	vec3 fogClr = mix(nFogColor, nRayClr, lightFactor * TimeDay);
			 fogClr = mix(fogClr, nRayClr, lightFactor * 0.1 * TimeMidnight);

	float minFogDensity = 0.6;

	#ifndef volumetricLight
		minFogDensity = 1.0;
	#endif

	// Apply volumetric light
	fogFactor = fogFactor * minFogDensity * fogDensity + mix(vlRays * vlRaysStrength * 0.2, vlRays * vlRaysStrength, (1.0 - eyeAdapt) * TimeDay) * fogDensity;
	fogFactor = mix(0.0, fogFactor, mix(float(fLand), 1.0 - TimeMidnight * 0.5, lightFactor));

	clr = mix(clr.rgb, fogClr, min(fogFactor, 1.0));

	return clr;

}

// Clamps between 0 and 1, exists in HLSL
float saturate(float x) { return clamp(x, 0.0, 1.0); }
vec2  saturate(vec2  x) { return clamp(x, vec2(0.0), vec2(1.0)); }
vec3  saturate(vec3  x) { return clamp(x, vec3(0.0), vec3(1.0)); }
vec4  saturate(vec4  x) { return clamp(x, vec4(0.0), vec4(1.0)); }

//Waterfog made by Zombye.
vec3 waterFog(
	in vec3 color,
	in vec3 waterSurfacePos,
	in vec3 opaqueSurfacePos
) {
	vec3 absorptionCoefficient = vec3(1.47, 0.65, 0.3);
	vec3 scatteringCoefficient = vec3(0.0002, 0.005, 0.01);

	vec3 waterDepth  = vec3(distance(waterSurfacePos, opaqueSurfacePos));
	      waterDepth *= absorptionCoefficient / 4.5;

	vec3 visibility = exp(-waterDepth);

	vec3 fog  = vec3(1.0) * scatteringCoefficient;
	     fog *= ((isEyeInWater == 1 ? eyeBrightness.y / 240.0 : gaux3SkyLightmap) * sunColor * 20.0);

	return mix(fog, color, saturate(visibility));
}

float linearizeDepth(float depth) {
    return -1.0 / ((depth * 2.0 - 1.0) * gbufferProjectionInverse[2].w + gbufferProjectionInverse[3].w);
}


vec4 raytrace(vec3 reflectedSky, vec3 fragpos, vec3 rVector) {

	// By Chocapic13

	const int maxf = 6;				//number of refinements
	const float stp = 1.0;			//size of one step for raytracing algorithm
	const float ref = 0.07;			//refinement multiplier
	const float inc = 2.2;			//increasement factor at each step

  vec4 color = vec4(0.0);

	#ifdef reflections

		vec3 start = fragpos;
		vec3 vector = stp * rVector;

		fragpos += vector;
		vec3 tvector = vector;

		int sr = 0;


		for (int i = 0; i < 28; i++) {

			vec3 pos = cameraSpaceToScreenSpace(fragpos);
			if (pos.x < 0 || pos.x > 1 || pos.y < 0 || pos.y > 1 || pos.z < 0 || pos.z > 1.0) break;

				vec3 spos = vec3(pos.st, texture2DLod(depthtex1, pos.st, 0.0).r);
						 spos = cameraSpaceToWorldSpace(spos * 2.0 - 1.0);

				float err = distance(fragpos.xyz, spos.xyz);

				if (err < (reflectiveBlocks? pow(length(vector) * 1.5, 1.15) : pow(length(vector) * pow(length(tvector), 0.11), 1.1) * 1.1)) {

					sr++;

					if (sr >= maxf) {

						bool rLand = texture2DLod(depthtex1, pos.st, 0.0).x < comp;

						float border = clamp(1.0 - pow(cdist(pos.st), 10.0), 0.0, 1.0);

						color = vec4(texture2DLod(composite, pos.st, 0.0).rgb, 1.0);
						color.rgb *= maxColorRange;

						color.rgb = drawFog(color.rgb, fragpos.xyz, true);

						if (!rLand) color.rgb = reflectedSky;

						if (isEyeInWater == 1) color.rgb = waterFog(color.rgb, vec3(-linearizeDepth(texture2D(depthtex0, texcoord).r)), vec3(0.0)).rgb;

						color.a *= border;

						break;

					}

				tvector -= vector;
				vector *= ref;

			}

			vector *= inc;
			tvector += vector;
			fragpos = start + tvector;

		}

	#endif

  return color;

}

vec3 getReflection(vec3 clr, vec3 fragpos0, vec3 fragpos1, vec3 skyFragpos) {

	float reflectionStrength = 1.0;

	vec3 getNormal = normal;
	if (reflectiveBlocks || hand) getNormal = gaux2normal;

	vec3 reflectedVector0 = reflect(normalize(fragpos0.xyz), getNormal);
	vec3 reflectedVector1 = reflect(normalize(fragpos1.xyz), getNormal);
	vec3 reflectedSkyVector = reflect(normalize(skyFragpos.xyz), getNormal) * 500.0;

	if (!reflectiveBlocks) reflectionStrength *= mix(0.0, 1.0, mix(pow(specular.r, 2.2), 0.0, specular.b) + specular.g * specular.a * specular.b);

	// Make relfective blocks not fully relfective.
	if (texture2D(gaux1, texcoord.xy).a > 0.6 && stainedGlass) reflectionStrength = 0.0;

	#ifndef reflections
		if (reflectiveBlocks) reflectionStrength *= 0.5;
	#endif

	vec3 reflectedSky	= drawSky(reflectedSkyVector.xyz);
			 reflectedSky = drawSun(reflectedSky, reflectedSkyVector.xyz, true);
			 //reflectedSky = drawStars(reflectedSky.rgb, reflectedSkyVector.xyz);
			 reflectedSky = draw2DClouds(reflectedSky.rgb, reflectedSkyVector.xyz);
			 reflectedSky = drawFog(reflectedSky.rgb, reflectedVector1.xyz, true);


	float normalDotEye = dot(getNormal, normalize(fragpos1.xyz));
	float fresnel	= pow(1.0 + normalDotEye, 2.0);
				fresnel = mix(fresnel, 1.0, luma(drawSun(vec3(0.0), reflectedSkyVector.xyz, true)));

	vec4 reflection = raytrace(reflectedSky, fragpos1.xyz, reflectedVector1);
		   reflection.rgb = mix(reflectedSky * globalLightmap, reflection.rgb, reflection.a);

	clr.rgb = mix(clr.rgb, reflection.rgb, min(fresnel, 1.0) * reflectionStrength);

	return clr;

}

#include "waterNoise.glsl"

vec2 hash( vec2 p ){
	p = vec2( dot(p,vec2(127.1,311.7)),dot(p,vec2(269.5,183.3)));
	return fract(sin(p)*43758.5453);
}

float voronoi( in vec2 x ){
	vec2 n = floor( x );
	vec2 f = fract( x );
	
	float F1 = 4.0;
	float F2 = 4.0;
	
	for( int j=-1; j<=1; j++ )
		for( int i=-1; i<=1; i++ ){
			vec2 g = vec2(i,j);
			vec2 o = hash( n + g );

			o = 0.45 + 0.4*sin( frameTimeCounter*4 + 6.1415*o );	
			vec2 r = g - f + o;

		float d = 	dot(r,r);


		if( d<F1 ) { 
			F2 = F1; 
			F1 = d; 
		} else if( d<F2 ) {
			F2 = d;
		}
    }
	
	float c = F1;
	
    return c;
}

float waterWaves(vec3 worldPos) {

	float wave = 0.0;

	#if defined waterShader && defined waterRefraction

		float waveSpeed = 1.0;

		if (ice) waveSpeed = 0.0;

		//worldPos.x += sin(worldPos.z * 0.5 + frameTimeCounter * waveSpeed * 1.5) * 0.4;
		worldPos.z += worldPos.y;
		worldPos.x += worldPos.y;

		worldPos.z *= 0.8;
		worldPos.x *= 1.7;

		//if (ice) wave  = texture2D(noisetex, worldPos.xz * 0.015 + vec2(frameTimeCounter * 0.02 * waveSpeed * windSpeed)).x * 0.2;
		
		wave  = getWaves(worldPos.xyz * 0.00015) * 10.2;
		if (ice) wave  = texture2D(noisetex, worldPos.xz * 0.015 + vec2(frameTimeCounter * 0.02 * waveSpeed * windSpeed)).x * 0.2;
	#endif

	return wave;

}

vec3 getRefraction(vec3 clr, vec3 fragpos) {

	float	waterRefractionStrength = 0.0083713;
	#ifdef RGB_Offset
	float rgbOffset = 0.25;
	#else
	float rgbOffset = 0.0;
	#endif

	vec4 worldPos = gbufferModelViewInverse * vec4(fragpos, 1.0);

	vec2 waterTexcoord = texcoord.st;

	waterRefractionStrength *= mix(0.2, 1.0, exp(-pow(length(fragpos.xyz) * 0.04, 1.5)));
	rgbOffset *= waterRefractionStrength;

	#ifdef waterRefraction

		float deltaPos = 0.1;
		float h0 = waterWaves(worldPos.xyz + cameraPosition.xyz);
		float h1 = waterWaves(worldPos.xyz + cameraPosition.xyz - vec3(deltaPos, 0.0, 0.0));
		float h2 = waterWaves(worldPos.xyz + cameraPosition.xyz - vec3(0.0, 0.0, deltaPos));

		float dX = (h0 - h1) / deltaPos;
		float dY = (h0 - h2) / deltaPos;

		vec3 waterRefract = normalize(vec3(dX, dY, 1.0));

		waterTexcoord = texcoord.st + waterRefract.xy * waterRefractionStrength;

		float mask = texture2D(gaux3, waterTexcoord.st).b;
		bool watermask = mask > 0.09 && mask < 0.1 || mask > 0.19 && mask < 0.21;

		waterTexcoord.st = watermask? waterTexcoord.st : texcoord.st;

		vec3 watercolor   = vec3(0.0);
				 watercolor.r = texture2DLod(composite, waterTexcoord.st + rgbOffset, 0.0).r;
				 watercolor.g = texture2DLod(composite, waterTexcoord.st, 0.0).g;
				 watercolor.b = texture2DLod(composite, waterTexcoord.st - rgbOffset, 0.0).b;
		 float depthInWater1 = texture2D(depthtex1, waterTexcoord.st).x;

	 	bool skyInWater	= depthInWater1 > comp;

		clr = skyInWater? clr : water || ice? watercolor * maxColorRange : clr;

	#endif

	return clr;

}

vec3 drawGAUX1(vec3 clr) {

	vec4 aColor = texture2D(gaux1, texcoord.xy) * vec4(texture2D(gcolor, texcoord.st).rgb, 1.0);
			 aColor.rgb *= maxColorRange;

	// Water shouldn't been redrawn.
	#ifdef waterShader
		if (water) aColor = vec4(clr.rgb, 1.0);
	#endif

	return mix(clr, aColor.rgb, aColor.a) + aColor.rgb * (1.0 - aColor.a);

}

void main() {

	const bool compositeMipmapEnabled = true;

	// Get main color.
	vec3 color = texture2D(composite, texcoord.st).rgb * maxColorRange;

	vec4 fragposition0  = gbufferProjectionInverse * (vec4(texcoord.st, depth0, 1.0) * 2.0 - 1.0);
       fragposition0 /= fragposition0.w;

	vec4 fragposition1  = gbufferProjectionInverse * (vec4(texcoord.st, depth1, 1.0) * 2.0 - 1.0);
	     fragposition1 /= fragposition1.w;

	vec4 skyFragposition  = gbufferProjectionInverse * (vec4(texcoord.st, 1.0, 1.0) * 2.0 - 1.0);
	     skyFragposition /= skyFragposition.w;

    float waterDepth = linearizeDepth(texture2D(depthtex0, texcoord).r) - linearizeDepth(texture2D(depthtex1, texcoord).r);

if (isEyeInWater == 0) {
	if (sky && !water) color.rgb = drawSky(skyFragposition.xyz);
	if (sky && !water) color.rgb = drawStars(color.rgb, skyFragposition.xyz);
	if (sky && !water) color.rgb = drawSun(color.rgb, skyFragposition.xyz, false);
	if (sky && !water) color.rgb = draw2DClouds(color.rgb, skyFragposition.xyz);
} else {
	if (sky) color.rgb = drawSky(skyFragposition.xyz);
	if (sky) color.rgb = drawStars(color.rgb, skyFragposition.xyz);
	if (sky) color.rgb = drawSun(color.rgb, skyFragposition.xyz, false);
	if (sky) color.rgb = draw2DClouds(color.rgb, skyFragposition.xyz);
}
	if (land) color.rgb *= 1.0 - specular.a * 0.25 * wetness;
	color.rgb = getRefraction(color.rgb, fragposition0.xyz);
	if (!water) color.rgb = drawFog(color.rgb, fragposition1.xyz, false);
	color.rgb = drawGAUX1(color.rgb);
	if (GAUX1 && !hand && !raindrops) color.rgb = drawFog(color.rgb, fragposition0.xyz, false);
	if(isEyeInWater == 0) color.rgb = getReflection(color.rgb, fragposition1.xyz, fragposition0.xyz, skyFragposition.xyz);
	if (isEyeInWater == 1) color.rgb = waterFog(color.rgb, vec3(-linearizeDepth(texture2D(depthtex0, texcoord).r)), vec3(0.0)).rgb;
	color.rgb = color.rgb * autoExpsoure();

/* DRAWBUFFERS:3 */

	gl_FragData[0] = vec4(color.rgb / maxColorRange, 1.0);

}
