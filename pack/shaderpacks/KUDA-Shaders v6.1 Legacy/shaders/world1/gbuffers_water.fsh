#version 120

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

#define waterShader
	#define parallaxWater
#define windSpeed 1.0 // [0.1 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6]

//#define animateUsingWorldTime

varying vec4 color;
varying vec4 position2;
varying vec4 worldposition;
varying vec3 tangent;
varying vec3 normal;
varying vec3 viewVector;
varying vec3 binormal;
varying vec2 texcoord;
varying vec2 lmcoord;
varying float water;
varying float ice;
varying float stainedGlass;
varying float stainedGlassPlane;
varying float netherPortal;
varying float dist;

uniform sampler2D texture;
uniform sampler2D noisetex;

uniform float frameTimeCounter;
uniform int worldTime;

#ifdef animateUsingWorldTime
	#define frameTimeCounter worldTime * 0.0416
#endif

float waterWaves(vec3 worldPos) {

	float wave = 1.0;

	#ifdef waterShader

		float waveSpeed = 1.0;

		if (ice > 0.9) waveSpeed = 0.0;

		//worldPos.x += sin(worldPos.z * 0.5 + frameTimeCounter * waveSpeed * 1.5) * 0.4;
		worldPos.z += worldPos.y;
		worldPos.x += worldPos.y;

		worldPos.z *= 0.7;
		worldPos.x *= 1.2;

		wave  = texture2D(noisetex, worldPos.xz * 0.015 + vec2(frameTimeCounter * 0.02 * waveSpeed * windSpeed)).x * 0.2;
		wave += sin((worldPos.x + worldPos.z * 0.5)	* 4.0 - frameTimeCounter * 6.0 * waveSpeed) * 0.03;
		wave += sin((worldPos.x + worldPos.z * 0.2)	* 2.0 - frameTimeCounter * 3.0 * waveSpeed) * 0.05;
		wave += sin((worldPos.x + worldPos.z * 0.5) * 1.0 - frameTimeCounter * 1.0 * waveSpeed) * 0.08;

		wave += sin((worldPos.x - worldPos.z) 			* 2.0 - frameTimeCounter * 6.0 * waveSpeed) * 0.05;
		wave += sin((worldPos.x - worldPos.z) 			* 1.0 - frameTimeCounter * 3.0 * waveSpeed) * 0.08;

	#endif

	return wave * 0.2;

}


vec3 waterwavesToNormal(vec3 pos) {

  float deltaPos = 0.2;
	float h0 = waterWaves(pos.xyz);
	float h1 = waterWaves(pos.xyz + vec3(deltaPos, 0.0, 0.0));
	float h2 = waterWaves(pos.xyz + vec3(-deltaPos, 0.0, 0.0));
	float h3 = waterWaves(pos.xyz + vec3(0.0, 0.0, deltaPos));
	float h4 = waterWaves(pos.xyz + vec3(0.0, 0.0, -deltaPos));

	float xDelta = ((h1 - h0) + (h0 - h2)) / deltaPos;
	float yDelta = ((h3 - h0) + (h0 - h4)) / deltaPos;

	return normalize(vec3(xDelta, yDelta, 1.0 - xDelta * xDelta - yDelta * yDelta));

}

vec3 getParallaxDisplacement(vec3 posxz) {

    vec3 parallaxPos = posxz;

		#ifdef waterShader

			#ifdef parallaxWater

		    float waterHeight = waterWaves(posxz);

				int samples = 6;
				float depth = 8.0;

		    for (int i = 0; i < 6; i++) {

		        parallaxPos.xz += waterHeight * viewVector.xy / dist * (1.0 / float(samples)) * depth;
		        waterHeight = waterWaves(parallaxPos);

		    }

			#endif

		#endif

    return parallaxPos;
}

vec4 normalMapping() {

	float bumpMult = 1.0;

	float NdotE = abs(dot(normal, normalize(position2.xyz)));

	bumpMult *= NdotE;

  vec4 result = vec4(vec3(normal) * 0.5 + 0.5, 1.0);

  if (water > 0.9 || ice > 0.9) {

  	vec3 bump  = waterwavesToNormal(getParallaxDisplacement(worldposition.xyz));
  			 bump *= vec3(bumpMult) + vec3(0.0, 0.0, 1.0 - bumpMult);

  	mat3 tbnMatrix = mat3(tangent.x, binormal.x, normal.x,
  						  					tangent.y, binormal.y, normal.y,
  						  					tangent.z, binormal.z, normal.z);

	  result = vec4(normalize(bump * tbnMatrix) * 0.5 + 0.5, 1.0);

  }

  return result;

}

vec4 toYCoCg(vec4 clr) {

	vec3 YCoCg = vec3(0.0);

	YCoCg.r =  0.25	* clr.r + 0.5 * clr.g + 0.25 * clr.b;
	YCoCg.g =  0.5	* clr.r - 0.5 * clr.b + 0.5;
	YCoCg.b = -0.25	* clr.r + 0.5 * clr.g - 0.25 * clr.b + 0.5;

	bool pattern = mod(gl_FragCoord.x, 2.0) == mod(gl_FragCoord.y, 2.0);

	YCoCg.g = pattern? YCoCg.b : YCoCg.g;

	return vec4(YCoCg, clr.a);

}


void main() {

  vec4 baseColor = texture2D(texture, texcoord.st) * color;
			 //baseColor = toYCoCg(baseColor);

	// This is actually for the water which is behind translucent blocks.
	#ifdef waterShader
  	if (water > 0.9) baseColor = vec4(vec3(0.3, 0.65, 1.0) * 0.3, 1.0);
	#endif



  float material = 0.01;
  if (water > 0.9) material = 0.1;
  if (ice > 0.9) material = 0.2;
  if (stainedGlass > 0.9) material = 0.3;
  if (stainedGlassPlane > 0.9) material = 0.3;
	//if (netherPortal > 0.9) material = 0.4;

/* DRAWBUFFERS:465 */

    // 0 = gcolor
    // 1 = gdepth
    // 2 = gnormal
    // 3 = composite
    // 4 = gaux1
    // 5 = gaux2
    // 6 = gaux3
    // 7 = gaux4

  gl_FragData[0] = baseColor;
  gl_FragData[1] = vec4(lmcoord.t, lmcoord.s, material, 1.0);
  gl_FragData[2] = normalMapping();

}
