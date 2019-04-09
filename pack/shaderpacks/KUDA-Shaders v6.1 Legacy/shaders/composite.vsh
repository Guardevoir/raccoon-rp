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

varying vec3 sunVector;
varying vec3 moonVector;
varying vec3 lightVector;

varying vec4 timeVector;

varying vec2 texcoord;
varying float weatherRatio;

uniform float sunAngle;

uniform vec3 sunPosition;
uniform vec3 moonPosition;

varying vec3 ambientColor;
varying vec3 sunlightColor;
varying vec3 underwaterColor;
varying vec3 shallowWaterColor;
varying vec3 torchColor;
varying vec3 waterColor;
varying vec3 lowlightColor;

varying float TimeBeforeSunrise;
varying float TimeSunrise;
varying float TimeSunrise2;
varying float TimeNoon;
varying float TimeSunset;
varying float TimeSunset2;
varying float TimeAfterSunset;
varying float TimeMidnight;
varying float TimeMidnight2;
varying float TimeDay;
varying float DayToNightFading;

uniform sampler2D noisetex;

uniform float rainStrength;
uniform float frameTimeCounter;

uniform int worldTime;
uniform int moonPhase;

uniform vec3 shadowLightPosition;

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

  float time = worldTime;
  TimeSunrise		= ((clamp(time, 22000.0, 24000.0) - 22000.0) / 2000.0) + (1.0 - (clamp(time, 0.0, 3000.0)/3000.0));
  TimeNoon			= ((clamp(time, 0.0, 3000.0)) / 3000.0) - ((clamp(time, 9000.0, 12000.0) - 9000.0) / 3000.0);
  TimeSunset		= ((clamp(time, 9000.0, 12000.0) - 9000.0) / 3000.0) - ((clamp(time, 12000.0, 14000.0) - 12000.0) / 2000.0);
  TimeMidnight	= ((clamp(time, 12000.0, 14000.0) - 12000.0) / 2000.0) - ((clamp(time, 22000.0, 24000.0) - 22000.0) / 2000.0);

  TimeDay			  = TimeSunrise + TimeNoon + TimeSunset;


  TimeBeforeSunrise	= ((clamp(time, 23250.0, 23255.0) - 23250.0) / 5.0) - ((clamp(time, 23255.0, 24000.0) - 23255.0) / 745.0);
  TimeSunrise2		  = ((clamp(time, 23255.0, 24000.0) - 23255.0) / 745.0) + (1.0 - (clamp(time, 0.0, 3000.0)/3000.0));
  TimeSunset2		    = ((clamp(time, 9000.0, 12000.0) - 9000.0) / 3000.0) - ((clamp(time, 12000.0, 12750.0) - 12000.0) / 750.0);
  TimeAfterSunset	  = ((clamp(time, 12000.0, 12750.0) - 12000.0) / 750.0) - ((clamp(time, 12750.0, 12755.0) - 12750.0) / 5.0);
  TimeMidnight2		  = ((clamp(time, 12750.0, 12755.0) - 12750.0) / 5.0) - ((clamp(time, 23250.0, 23255.0) - 23250.0) / 5.0);


  DayToNightFading	= 1.0 - (clamp((time - 12000.0) / 750.0, 0.0, 1.0) - clamp((time - 12750.0) / 750.0, 0.0, 1.0)
  							          +  clamp((time - 22000.0) / 750.0, 0.0, 1.0) - clamp((time - 23250.0) / 750.0, 0.0, 1.0));

  if (time < 12700 || time > 23250) {
		lightVector = normalize(sunPosition);
	} else {
		lightVector = normalize(moonPosition);
	}

  weatherRatio = getWeatherRatio();

  ambientColor  = vec3(0.0);
  ambientColor += vec3(0.85, 0.9, 1.0)	* 0.6		* TimeSunrise;
  ambientColor += vec3(0.85, 0.9, 1.0)					* TimeNoon;
  ambientColor += vec3(0.85, 0.9, 1.0)	* 0.6		* TimeSunset;
  ambientColor += vec3(0.6, 0.7, 1.0)	  * 0.04	* TimeMidnight;

  ambientColor *= 1.0 - weatherRatio;
  ambientColor += vec3(1.0, 1.0, 1.0)		* 0.8		* TimeSunrise		* weatherRatio;
  ambientColor += vec3(1.0, 1.0, 1.0)		* 0.9		* TimeNoon			* weatherRatio;
  ambientColor += vec3(1.0, 1.0, 1.0)		* 0.8		* TimeSunset		* weatherRatio;
  ambientColor += vec3(0.65, 0.8, 1.0)		* 0.04 	* TimeMidnight	* weatherRatio;

  sunlightColor  = vec3(0.0);
  sunlightColor += blackbody(3200) * vec3(1.0, 0.3, 0.)	  * 0.5		* TimeBeforeSunrise;
  sunlightColor += blackbody(3700)	* 0.2		* TimeSunrise2;
  sunlightColor += blackbody(5400)	* 0.3			  * TimeNoon;
  sunlightColor += blackbody(3700)	* 0.2		* TimeSunset2;
  sunlightColor += blackbody(3200) * vec3(1.0, 0.3, 0.)	  * 0.5		* TimeAfterSunset;
  sunlightColor += blackbody(5200) * vec3(0.65, 0.8, 1.0)	* 0.03	* TimeMidnight2;

  shallowWaterColor  = vec3(0.0);
  shallowWaterColor += vec3(0.1, 1.6, 1.0)	* 0.3		* TimeSunrise;
  shallowWaterColor += vec3(0.0, 1.2, 1.0)					* TimeNoon;
  shallowWaterColor += vec3(0.1, 1.6, 1.0)	* 0.3		* TimeSunset;
  shallowWaterColor += vec3(0.1, 1.45, 1.0)	* 0.08	* TimeMidnight;

  underwaterColor  = vec3(0.0);
  underwaterColor += vec3(0.1, 1.4, 1.0)	* 0.3		* TimeSunrise;
  underwaterColor += vec3(0.1, 1.0, 1.0)					* TimeNoon;
  underwaterColor += vec3(0.1, 1.4, 1.0)	* 0.3		* TimeSunset;
  underwaterColor += vec3(0.1, 0.45, 1.0)	* 0.08	* TimeMidnight;

  torchColor = blackbody(3773.15);

  waterColor = vec3(0.98, 0.98, 1.0);

  lowlightColor = vec3(0.65, 0.8, 1.0);

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
