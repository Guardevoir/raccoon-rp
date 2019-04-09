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

//#define dynamicWeather
  #define weatherRatioSpeed	1.0 // [0.1 0.5 1.0 2.0 5.0 10.0] Won't take any effect when 'useMoonPhases' is enabled!
  //#define useMoonPhases

//#define useFixedWeatherRatio
  #define cloudCover 0.5 // [0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]

#define minimumLight

float pow2(in float n)  { return n * n; }
float pow3(in float n)  { return pow2(n) * n; }
float pow4(in float n)  { return pow2(pow2(n)); }
float pow5(in float n)  { return pow2(pow2(n)) * n; }
float pow6(in float n)  { return pow2(pow2(n) * n); }
float pow7(in float n)  { return pow2(pow2(n) * n) * n; }
float pow8(in float n)  { return pow2(pow2(pow2(n))); }
float pow9(in float n)  { return pow2(pow2(pow2(n))) * n; }
float pow10(in float n) { return pow2(pow2(pow2(n)) * n); }
float pow11(in float n) { return pow2(pow2(pow2(n)) * n) * n; }
float pow12(in float n) { return pow2(pow2(pow2(n) * n)); }
float pow13(in float n) { return pow2(pow2(pow2(n) * n)) * n; }
float pow14(in float n) { return pow2(pow2(pow2(n) * n) * n); }
float pow15(in float n) { return pow2(pow2(pow2(n) * n) * n) * n; }
float pow16(in float n) { return pow2(pow2(pow2(pow2(n)))); }

#define max0(n) max(0.0, n)
#define min1(n) min(1.0, n)
#define clamp01(n) clamp(n, 0.0, 1.0)

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

uniform sampler2D noisetex;

uniform float sunAngle;

uniform vec3 sunPosition;
uniform vec3 moonPosition;

uniform vec3 shadowLightPosition;

uniform float rainStrength;
uniform float frameTimeCounter;
uniform float nightVision;
uniform float screenBrightness;

uniform int worldTime;
uniform int moonPhase;



float getWeatherRatio() {

  float value = rainStrength;

  #ifdef dynamicWeather

    #ifdef useMoonPhases

      value = float(moonPhase) / 7.0;

    #else

  	 value = pow(texture2D(noisetex, vec2(1.0) + vec2(frameTimeCounter * 0.005) * 0.01 * weatherRatioSpeed).x, 2.0);

    #endif

  #endif

  #ifdef useFixedWeatherRatio

    value = cloudCover;

  #endif

  // Raining.
  value = mix(value, 1.0, rainStrength);

	return pow(value, mix(2.0, 1.0, rainStrength));

}

//Credit to Jodie for allowing KUDA to have this
vec3 blackbody(float t){
    // http://en.wikipedia.org/wiki/Planckian_locus

    vec4 vx = vec4(-0.2661239e9,-0.2343580e6,0.8776956e3,0.179910);
    vec4 vy = vec4(-1.1063814,-1.34811020,2.18555832,-0.20219683);
    float it = 1./t;
    float it2= it*it;
    float x = dot(vx,vec4(it*it2,it2,it,1.));
    float x2 = x*x;
    float y = dot(vy,vec4(x*x2,x2,x,1.));
    float z = 1. - x - y;
    
    // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    mat3 xyzToSrgb = mat3(
         3.2404542,-1.5371385,-0.4985314,
        -0.9692660, 1.8760108, 0.0415560,
         0.0556434,-0.2040259, 1.0572252
    );

    vec3 srgb = vec3(x/y,1.,z/y) * xyzToSrgb;
    return max(srgb,0.);
}

void main() {

	texcoord = gl_MultiTexCoord0.st;

	gl_Position = ftransform();

	if (float(worldTime) < 12700 || float(worldTime) > 23250) {
		lightVector = normalize(sunPosition);
	} else {
		lightVector = normalize(moonPosition);
	}

  float time = worldTime;
  TimeSunrise		= ((clamp(time, 22000.0, 24000.0) - 22000.0) / 2000.0) + (1.0 - (clamp(time, 0.0, 3000.0)/3000.0));
  TimeNoon			= ((clamp(time, 0.0, 3000.0)) / 3000.0) - ((clamp(time, 9000.0, 12000.0) - 9000.0) / 3000.0);
  TimeSunset		= ((clamp(time, 9000.0, 12000.0) - 9000.0) / 3000.0) - ((clamp(time, 12000.0, 14000.0) - 12000.0) / 2000.0);
  TimeMidnight	= ((clamp(time, 12000.0, 14000.0) - 12000.0) / 2000.0) - ((clamp(time, 22000.0, 24000.0) - 22000.0) / 2000.0);

  TimeDay			  = TimeSunrise + TimeNoon + TimeSunset;

  DayToNightFading	= 1.0 - (clamp((time - 12000.0) / 750.0, 0.0, 1.0) - clamp((time - 12750.0) / 750.0, 0.0, 1.0)
  							          +  clamp((time - 22000.0) / 750.0, 0.0, 1.0) - clamp((time - 23250.0) / 750.0, 0.0, 1.0));

	weatherRatio = getWeatherRatio();
  float minLight = screenBrightness + nightVision * 2.0;

  #ifdef minimumLight
    minLight += 1.0;
  #endif

  // Set up colors.
	rayColor  = vec3(0.0);
	rayColor += vec3(1.0, 0.7, 0.5) 	* 2.0 	* TimeSunrise;
	rayColor += vec3(1.0, 1.0, 1.0) 	* 2.0		* TimeNoon;
	rayColor += vec3(1.0, 0.7, 0.5) 	* 2.0		* TimeSunset;
	rayColor += vec3(0.65, 0.8, 1.0) 					* TimeMidnight * max(minLight, 1.0);

	sunColor  = vec3(0.0);
	sunColor += blackbody(4400) 	* 2.0	* TimeSunrise;
	sunColor += blackbody(5400) 	* 2.0	* TimeNoon;
	sunColor += blackbody(4400) 	* 2.0	* TimeSunset;
	sunColor += blackbody(5200) * vec3(1.0, 0.45, 0.2)				* TimeMidnight;

	moonColor = vec3(1.0);

  skyColor  = vec3(0.0);
  skyColor += vec3(0.7, 0.85, 1.0)	 * 0.7		* TimeSunrise;
  skyColor += vec3(0.65, 0.85, 1.0)		  		  * TimeNoon;
  skyColor += vec3(0.7, 0.85, 1.0)	 * 0.7		* TimeSunset;
  skyColor += vec3(0.65, 0.8, 1.0)	 * 0.03	  * TimeMidnight * max(minLight, 1.0);


  skyColor *= 1.0 - weatherRatio;
  skyColor += vec3(1.0, 1.0, 1.0)	  * 0.7	 	* TimeSunrise		* weatherRatio;
  skyColor += vec3(1.0, 1.0, 1.0)   * 0.8	 	* TimeNoon			* weatherRatio;
  skyColor += vec3(1.0, 1.0, 1.0)	  * 0.7	 	* TimeSunset		* weatherRatio;
  skyColor += vec3(0.65, 0.8, 1.0)	* 0.03	* TimeMidnight	* weatherRatio * max(minLight, 1.0);

  horizonColor  = vec3(0.0);
  horizonColor += vec3(1.0, 0.65, 0.5)	* 0.9		* TimeSunrise;
  horizonColor += vec3(1.0, 0.97, 0.93)	  		  * TimeNoon;
  horizonColor += vec3(1.0, 0.65, 0.5)	* 0.9		* TimeSunset;
  horizonColor += vec3(0.75, 0.8, 1.0) 	* 0.06	* TimeMidnight * max(minLight, 1.0);

  horizonColor *= 1.0 - weatherRatio;
  horizonColor += vec3(1.0, 0.9, 0.8)		* 0.9		* TimeSunrise		* weatherRatio;
  horizonColor += vec3(1.0, 1.0, 1.0)		* 0.8		* TimeNoon			* weatherRatio;
  horizonColor += vec3(1.0, 0.9, 0.8)		* 0.9		* TimeSunset		* weatherRatio;
  horizonColor += vec3(0.65, 0.8, 1.0) 	* 0.045	* TimeMidnight	* weatherRatio * max(minLight, 1.0);

  fogColor  = vec3(0.0);
  fogColor += vec3(1.0, 1.0, 1.0) 	  * 0.7		* TimeSunrise;
  fogColor += vec3(0.85, 0.9, 1.0)		* 0.9		* TimeNoon;
  fogColor += vec3(1.0, 1.0, 1.0) 	  * 0.7		* TimeSunset;
  fogColor += vec3(0.65, 0.8, 1.0)		* 0.04	* TimeMidnight * max(minLight, 1.0);

  fogColor *= 1.0 - weatherRatio;
  fogColor += vec3(1.0, 0.95, 0.9)  * 0.9		* TimeSunrise  * weatherRatio;
  fogColor += vec3(1.0, 1.0, 1.0) 	* 1.0		* TimeNoon     * weatherRatio;
  fogColor += vec3(1.0, 0.95, 0.9)  * 0.9		* TimeSunset   * weatherRatio;
  fogColor += vec3(0.65, 0.8, 1.0)	* 0.04 	* TimeMidnight * weatherRatio * max(minLight, 1.0);

  underwaterColor  = vec3(0.0);
  underwaterColor += vec3(0.1, 1.4, 1.0)	* 0.4		* TimeSunrise;
  underwaterColor += vec3(0.0, 0.65, 1.0)					* TimeNoon;
  underwaterColor += vec3(0.1, 1.4, 1.0)	* 0.4		* TimeSunset;
  underwaterColor += vec3(0.1, 0.45, 1.0)	* 0.08	* TimeMidnight;

  cloudColor  = vec3(0.0);
  cloudColor += vec3(1.0, 0.8, 0.65)		* 0.9		* TimeSunrise;
  cloudColor += vec3(1.0, 1.0, 1.0)							* TimeNoon;
  cloudColor += vec3(1.0, 0.8, 0.65)		* 0.9	  * TimeSunset;
  cloudColor += vec3(0.65, 0.8, 1.0)	  * 0.05	* TimeMidnight * max(minLight, 1.0);

  cloudColor *= 1.0 - weatherRatio;
  cloudColor += vec3(1.0, 0.95, 0.9)	* 0.6		* TimeSunrise		* weatherRatio;
  cloudColor += vec3(1.0, 1.0, 1.0)		* 0.8		* TimeNoon			* weatherRatio;
  cloudColor += vec3(1.0, 0.95, 0.9)	* 0.6		* TimeSunset		* weatherRatio;
  cloudColor += vec3(0.65, 0.8, 1.0)	* 0.03	* TimeMidnight	* weatherRatio * max(minLight, 1.0);

  sunVector = normalize(sunPosition); 
  moonVector = normalize(moonPosition); 
  lightVector = normalize(shadowLightPosition);

  lightVector = (sunAngle > 0.5) ? moonVector : sunVector;

    vec2 noonNight   = vec2(0.0);
     noonNight.x = (0.25 - clamp(sunAngle, 0.0, 0.5));
     noonNight.y = (0.75 - clamp(sunAngle, 0.5, 1.0));

    // NOON
    timeVector.x = 1.0 - clamp01(pow2(abs(noonNight.x) * 4.0));
    // NIGHT
    timeVector.y = 1.0 - clamp01(pow(abs(noonNight.y) * 4.0, 128.0));
    // SUNRISE/SUNSET
    timeVector.z = 1.0 - (timeVector.x + timeVector.y);
    // MORNING
    timeVector.w = 1.0 - ((1.0 - clamp01(pow2(max0(noonNight.x) * 4.0))) + (1.0 - clamp01(pow(max0(noonNight.y) * 4.0, 128.0))));
}
