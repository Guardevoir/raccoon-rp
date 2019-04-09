#version 120
#extension GL_EXT_gpu_shader4 : enable

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

//#define ambientOcclusion	// Experimental! Make sure to set ambientOcclusionLevel to 0!
#define minimumLight
#define torchlightBrightness 1.0 // [0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0]
#define torchlightRadius 1.0 // [0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0]
//#define YCoCg_Compression

#define maxColorRange 20.0

varying vec3 lightVector;
varying vec2 texcoord;

varying vec3 ambientColor;
varying vec3 underwaterColor;
varying vec3 torchColor;
varying vec3 waterColor;
varying vec3 lowlightColor;

uniform sampler2DShadow shadow;		// This is just to prevent rendering entity shadows.

uniform sampler2D gcolor;
uniform sampler2D gaux1;
uniform sampler2D gaux2;
uniform sampler2D gaux3;
uniform sampler2D gdepth;
uniform sampler2D gnormal;
uniform sampler2D depthtex1;
uniform sampler2D depthtex0;
uniform sampler2D noisetex;

uniform mat4 gbufferProjection;
uniform mat4 gbufferModelViewInverse;
uniform mat4 gbufferProjectionInverse;

uniform vec3 sunPosition;
uniform vec3 moonPosition;
uniform vec3 upPosition;

uniform float near;
uniform float far;
uniform float rainStrength;
uniform float centerDepthSmooth;
uniform float viewWidth;
uniform float viewHeight;
uniform float screenBrightness;
uniform float nightVision;

uniform int worldTime;
uniform int isEyeInWater;

uniform ivec2 eyeBrightnessSmooth;

const int 		RGB16 									 = 2;
const int 		RGBA16 									 = 2;

const int 		compositeFormat 				 = RGBA16;
const int 		gcolorFormat 					 	 = RGBA16;
const int 		gaux4Format 						 = RGBA16;

const int 		gaux2Format 					 	 = RGB16;
const int 		gnormalFormat 					 = RGB16;

// Constants
const float		eyeBrightnessHalflife 	 = 7.5f;
const float 	centerDepthHalflife 		 = 2.0f;
const float		ambientOcclusionLevel		 = 0.7f;
const float 	wetnessHalflife					 = 600.0f;
const float 	drynessHalflife					 = 200.0f;
const int			noiseTextureResolution	 = 128;

vec3  normal = texture2D(gnormal, texcoord.st).rgb * 2.0 - 1.0;
vec3  gaux2normal = texture2D(gaux2, texcoord.st).rgb * 2.0 - 1.0;
float depth0 = texture2D(depthtex0, texcoord.st).x;
float depth1 = texture2D(depthtex1, texcoord.st).x;
float skyLightmap = clamp(pow(texture2D(gdepth, texcoord.st).r, 2.0), 0.0, 1.0);
float torchLightmap = clamp(texture2D(gdepth, texcoord.st).g, 0.0, 1.0);
float material = texture2D(gdepth, texcoord.st).b;

float gaux3SkyLightmap = clamp(pow(texture2D(gaux3, texcoord.st).r, 2.0), 0.0, 1.0);
float gaux3TorchLightmap = clamp(texture2D(gaux3, texcoord.st).g, 0.0, 1.0);
float gaux3Material = texture2D(gaux3, texcoord.st).b;

float comp = 1.0 - near / far / far;

bool land	= depth1 < comp;
bool sky	= depth1 > comp;

bool armorGlint = material > 0.49 && material < 0.51;
bool emissiveLight = material > 0.09 && material < 0.11;
bool emissiveHandlight = gaux3Material > 0.59 && gaux3Material < 0.61;
bool water = gaux3Material > 0.09 && gaux3Material < 0.11;
bool ice = gaux3Material > 0.19 && gaux3Material < 0.21;
bool stainedGlass = gaux3Material > 0.29 && gaux3Material < 0.31;
bool hand = gaux3Material > 0.49 && gaux3Material < 0.51;
bool GAUX1 = gaux3Material > 0.0;		// Ask for all materials which are stored in gaux3.

float luma(vec3 clr) {
	return dot(clr, vec3(0.3333));
}

float getTorchLightmap(float lightmap, float skyL) {

	float tRadius = 3.0;	// Higher means lower.
	float tBrightness = 0.5;

	return min(pow(lightmap, tRadius / torchlightRadius) * torchlightBrightness * tBrightness, 1.0);

}

vec3 doEmissiveLight(vec3 clr, vec3 originalClr, bool forHand) {

	float exposure	= 2.5;
	float cover		= 0.4;

	if (forHand) emissiveLight = emissiveHandlight;
	if (emissiveLight) clr = mix(clr.rgb, vec3(1.0) * exposure, max(luma(originalClr.rgb) - cover, 0.0));

	return clr;

}

vec3 lowlightEye(vec3 clr) {

	float desaturationAmount = 0.7;

	desaturationAmount *= mix(1.0, 0.0, torchLightmap);

	return mix(clr, vec3(luma(clr)) * lowlightColor, desaturationAmount);

}

#ifdef ambientOcclusion

	vec3 toScreenSpace(vec2 p) {
			vec4 fragposition = gbufferProjectionInverse * vec4(vec3(p, texture2D(depthtex1,p).x) * 2. - 1., 1.);
			return fragposition.xyz /= fragposition.w;
	}

	int bitfieldReverse(int a) {
		a = ((a & 0x55555555) << 1 ) | ((a & 0xAAAAAAAA) >> 1);
		a = ((a & 0x33333333) << 2 ) | ((a & 0xCCCCCCCC) >> 2);
		a = ((a & 0x0F0F0F0F) << 4 ) | ((a & 0xF0F0F0F0) >> 4);
		a = ((a & 0x00FF00FF) << 8 ) | ((a & 0xFF00FF00) >> 8);
		a = ((a & 0x0000FFFF) << 16) | ((a & 0xFFFF0000) >> 16);
		return a;
	}

	#define hammersley(i, N) vec2( float(i) / float(N), float( bitfieldReverse(i) ) * 2.3283064365386963e-10 )
	#define tau 6.2831853071795864769252867665590
	#define circlemap(p) (vec2(cos((p).y*tau), sin((p).y*tau)) * p.x)

#endif

float jaao(vec2 p) {

	// By Jodie.

	const float radius = 1.0;
	const int steps = 16;

	float ao = 1.0;

	#ifdef ambientOcclusion

		int x = int(p.x*viewWidth)  % 4;
		int y = int(p.y*viewHeight) % 4;
		int index = (x<<2) + y;

		vec3 p3 = toScreenSpace(p);
		vec3 normal = normalize( cross(dFdx(p3), dFdy(p3)) );
		vec2 clipRadius = radius * vec2(viewHeight/viewWidth,1.) / length(p3);

		vec3 v = normalize(-p3);

		float nvisibility = 0.;
		float vvisibility = 0.;

		for (int i = 0; i < steps; i++) {
			vec2 circlePoint = circlemap(
				hammersley(i*15+index+1, 16*steps)
			)*clipRadius;

			vec3 o  = toScreenSpace(circlePoint    +p) - p3;
			vec3 o2 = toScreenSpace(circlePoint*.25+p) - p3;
			float l  = length(o );
			float l2 = length(o2);
			o /=l ;
			o2/=l2;

			nvisibility += clamp(1.-max(
				dot(o , normal) - clamp((l -radius)/radius,0.,1.),
				dot(o2, normal) - clamp((l2-radius)/radius,0.,1.)
			), 0., 1.);

			vvisibility += clamp(1.-max(
				dot(o , v) - clamp((l -radius)/radius,0.,1.),
				dot(o2, v) - clamp((l2-radius)/radius,0.,1.)
			), 0., 1.);
		}

		ao = min(vvisibility*2., nvisibility) / float(steps);

	#endif

	return ao;

}

vec3 patternFilter(vec3 clr) {

	// By Chocapic13

	vec2 a0 = texture2D(gcolor, texcoord.st + vec2(1.0 / viewWidth,0.0)).rg;
	vec2 a1 = texture2D(gcolor, texcoord.st - vec2(1.0 / viewWidth,0.0)).rg;
	vec2 a2 = texture2D(gcolor, texcoord.st + vec2(0.0, 1.0 / viewHeight)).rg;
	vec2 a3 = texture2D(gcolor, texcoord.st - vec2(0.0, 1.0 / viewHeight)).rg;

	vec4 lumas = vec4(a0.x, a1.x, a2.x, a3.x);
	vec4 chromas = vec4(a0.y, a1.y, a2.y, a3.y);

	const vec4 threshold = vec4(30.0 / 255.0);

	vec4 w = 1.0 - step(threshold, abs(lumas - clr.x));
	float W = dot(w, vec4(1.0));

	w.x = W == 0.0? 1.0 : w.x;
	W = W == 0.0? 1.0 : W;

	float chroma = dot(w, chromas) / W;


	bool pattern = mod(gl_FragCoord.x, 2.0) == mod(gl_FragCoord.y, 2.0);

	clr.b = chroma;
	clr.rgb = pattern? clr.rbg : clr.rgb;

	return clr;

}

vec3 toRGB(vec3 clr){

	clr.y -= 0.5;
	clr.z -= 0.5;

	return vec3(clr.r + clr.g - clr.b, clr.r + clr.b, clr.r - clr.g - clr.b);

}


void main() {

	vec3 color = texture2D(gcolor, texcoord.st).rgb;

	#ifdef YCoCg_Compression

		color = patternFilter(color);
		color = armorGlint? color.rgb : toRGB(color.rgb);

	#endif

	vec4 fragposition0  = gbufferProjectionInverse * (vec4(texcoord.st, depth0, 1.0) * 2.0 - 1.0);
	     fragposition0 /= fragposition0.w;

	vec4 fragposition1  = gbufferProjectionInverse * (vec4(texcoord.st, depth1, 1.0) * 2.0 - 1.0);
	     fragposition1 /= fragposition1.w;


	float ao = jaao(texcoord);
	float minLight = screenBrightness + nightVision * 2.0;

	#ifdef minimumLight
		minLight += 1.0;
	#endif

  vec3 newTorchLightmap					= torchColor * getTorchLightmap(torchLightmap, skyLightmap);
	vec3 newGaux3TorchLightmap		= torchColor * getTorchLightmap(gaux3TorchLightmap, gaux3SkyLightmap);

  vec3 newLightmap = minLight * 0.1 * ambientColor * ao + newTorchLightmap;
			 newLightmap = doEmissiveLight(newLightmap, color.rgb, false);

  vec3 newGAUX1Lightmap = minLight * 0.1 * ambientColor * ao + newGaux3TorchLightmap;
			 newGAUX1Lightmap = doEmissiveLight(newGAUX1Lightmap, texture2D(gaux1, texcoord.xy).rgb, true);

	color.rgb = lowlightEye(color.rgb);
  color.rgb *= newLightmap;

	if (water) {

		float waterDepth = mix(1.0 - pow(texture2D(gdepth, texcoord.st).r, 1.3), 0.0, 1.0 - gaux3SkyLightmap);

		color.rgb = mix(color.rgb * waterColor, underwaterColor * 0.15, waterDepth);

	}


/* DRAWBUFFERS:30 */

  // 0 = gcolor
  // 1 = gdepth
  // 2 = gnormal
  // 3 = composite
  // 4 = gaux1
  // 5 = gaux2
  // 6 = gaux3
  // 7 = gaux4

  gl_FragData[0] = vec4(color.rgb / maxColorRange, 1.0);
	gl_FragData[1] = vec4(newGAUX1Lightmap / maxColorRange, 1.0);

}
