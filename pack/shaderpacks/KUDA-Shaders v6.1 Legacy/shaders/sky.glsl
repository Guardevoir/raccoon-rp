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

const float pi  = 3.14159265358979;

float getLuma(in vec3 colour) { return dot(colour, vec3(0.2125, 0.7154, 0.0721)); }
vec3 colourSaturation(in vec3 colour, in float saturation) { return colour * saturation + getLuma(colour) * (1.0 - saturation); }

#define DIRECT_MOONLIGHT_SATURATION 0.0

  vec3 moonLight = colourSaturation(vec3(0.0, 0.0, 1.0), DIRECT_MOONLIGHT_SATURATION) * 0.5;

#define atmosphereHeight 8000.  // actual thickness of the atmosphere
#define earthRadius 6371000.    // actual radius of the earth
#define mieMultiplier 1.
#define ozoneMultiplier 1.      // 1. for physically based 
#define rayleighDistribution 8. //physically based 
#define mieDistribution 1.8     //physically based 
#define rayleighCoefficient vec3(5.8e-6,1.35e-5,3.31e-5) // Physically based (Bruneton, Neyret)
#define ozoneCoefficient (vec3(3.426,8.298,.356) * 6e-5 / 100.) // Physically based (Kutz)
#define mieCoefficient ( 3e-6 * mieMultiplier) //good default

vec2 js_getThickness2(vec3 rd){
    vec2 sr = earthRadius + vec2(
        atmosphereHeight,
        atmosphereHeight * mieDistribution / rayleighDistribution
    );
    vec3 ro = -upVector * earthRadius;
    float b = dot(rd, ro);
    return b + sqrt( sr * sr + (b * b - dot(ro, ro)) );
}

#define getEarth(a) pow(smoothstep(-.1,.1,dot(upVector,a)),1.)
// Improved Rayleigh phase for single scattering (Elek)
#define phaseRayleigh(a) ( .4 * (a) + 1.14 )

float phaseMie(float x){
    const vec3 c = vec3(.256098,.132268,.010016);
    const vec3 d = vec3(-1.5,-1.74,-1.98);
    const vec3 e = vec3(1.5625,1.7569,1.9801);
    return dot((x * x + 1.) * c / pow( d * x + e, vec3(2.1, 2.4, 2.6)),vec3(.33333333333));
}

vec3 absorb(in vec2 a) { return exp( -(a).x * (  ozoneCoefficient * ozoneMultiplier + rayleighCoefficient) - 1.11 * (a).y * mieCoefficient); }

const float js_steps = 8.;
const float js_steps_inv = 2.0 / js_steps;

vec3 phaseRainbow(float x){

    vec3 a = x + vec3(-1,0,1) * 0.03 + 0.6;

    vec3 b = x + vec3(1,0,-1) * 0.03 + 0.3;

    return exp( -500. * a * a + 2.)+exp( -500. * b * b + 1.);
}

vec3 js_sunColor(vec3 V, vec3 L) {
    return absorb(js_getThickness2(L)) * getEarth(L);
}

float shdwcrv (float x){
    x = clamp(x,0.,1.);
    x = -x * x + 1.;
    x = -x * x + 1.;
    x = -x * x + 1.;
    x = -x * x + 1.;
    return x;
}

float sphSoftShadow(vec3 position,vec3 L){
    const float k = 10.;
    //vec4 sph = vec4(-up*earthRadius,earthRadius);
    vec3 oc = position + upVector * earthRadius;
    float b = dot( oc, L );
    float c = dot( oc, oc ) - earthRadius*earthRadius;
    float h = b*b - c;
    
    float d = -earthRadius + sqrt( max(0.0,earthRadius*earthRadius-h));
    float t = -b - sqrt( max(0.0,h) );
    return (t<0.0) ? 1.0 : shdwcrv(k*d/t) ;
}

vec3 js_getScatter(in vec3 colour, in vec3 V, in vec3 L, in int mode) {
  vec2 thickness = js_getThickness2(V) / js_steps;

  float dotVS = dot(V, sunVector);
  float dotVM = dot(V, moonVector);

  vec3 viewAbsorb = absorb(thickness);
  vec4 scatterCoeff = 1. - exp(-thickness.xxxy * vec4(rayleighCoefficient,mieCoefficient));

  vec3 scatterS = scatterCoeff.xyz * phaseRayleigh(dotVS) + (scatterCoeff.w * phaseMie(dotVS) * 0.0000275);
  vec3 scatterM = scatterCoeff.xyz * phaseRayleigh(dotVM) + (scatterCoeff.w * phaseMie(dotVM));

  vec3 js_sunAbsorb = absorb(js_getThickness2(sunVector)*js_steps_inv) * getEarth(sunVector);
  vec3 js_moonAbsorb = absorb(js_getThickness2(moonVector)*js_steps_inv) * getEarth(moonVector);
  
  vec3 skyColorS = mode != 0 ? vec3(0.0) : colour + (sin(max0(pow16(dotVS) - 0.9935) / 0.015 * pi) * js_sunAbsorb * 200.0);
  vec3 skyColorM = mode != 0 ? vec3(0.0) : colour + (max0(pow(dotVM, 5000.0)) * 50.0 * moonLight);

  float ertShdwSun = sphSoftShadow(thickness.x*V,sunVector);
  float ertShdwMoon = sphSoftShadow(thickness.x*V,moonVector);

  for(int i = 0; i < int(js_steps); i++) {
    scatterS *= js_sunAbsorb * 1.6;
    scatterM *= js_moonAbsorb * moonLight * 4.725;

    skyColorS = skyColorS * viewAbsorb + scatterS;
    skyColorM = skyColorM * viewAbsorb + scatterM;
  }

    return skyColorS * ertShdwSun + skyColorM * ertShdwMoon;;
}