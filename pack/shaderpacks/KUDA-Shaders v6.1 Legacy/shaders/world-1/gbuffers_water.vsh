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

//#define shakingCamera
//#define animateUsingWorldTime

varying vec4 color;
varying vec4 position2;
varying vec4 worldposition;
varying vec3 tangent;
varying vec3 normal;
varying vec3 binormal;
varying vec3 viewVector;
varying vec2 texcoord;
varying vec2 lmcoord;
varying float water;
varying float ice;
varying float stainedGlass;
varying float stainedGlassPlane;
varying float netherPortal;
varying float dist;

uniform mat4 gbufferModelView;
uniform mat4 gbufferModelViewInverse;

attribute vec4 mc_Entity;
attribute vec4 at_tangent;

uniform vec3 cameraPosition;

uniform float frameTimeCounter;
uniform int worldTime;

#ifdef animateUsingWorldTime
	#define frameTimeCounter worldTime * 0.0416
#endif


void main() {

  texcoord          = gl_MultiTexCoord0.st;
  lmcoord           = (gl_TextureMatrix[1] * gl_MultiTexCoord1).xy;
  normal            = normalize(gl_NormalMatrix * gl_Normal);
  color             = gl_Color;
  water             = 0.0;
  ice               = 0.0;
  stainedGlass      = 0.0;
  stainedGlassPlane = 0.0;
  tangent           = vec3(0.0);
	binormal          = vec3(0.0);

  position2 = gl_ModelViewMatrix * gl_Vertex;

  vec4 position = gbufferModelViewInverse * gl_ModelViewMatrix * gl_Vertex;

	worldposition = position + vec4(cameraPosition.xyz, 0.0);

  #ifdef shakingCamera
		position += vec4(0.02 * sin(frameTimeCounter * 2.0), 0.005 * cos(frameTimeCounter * 3.0), 0.0, 0.0) * gbufferModelView;
	#endif

	gl_Position = gl_ProjectionMatrix * gbufferModelView * position;

  if (mc_Entity.x == 8.0 || mc_Entity.x == 9.0) water = 1.0;
  if (mc_Entity.x == 79.0) ice = 1.0;
  if (mc_Entity.x == 90.0) netherPortal = 1.0;
  if (mc_Entity.x == 95.0) stainedGlass = 1.0;
  if (mc_Entity.x == 160.0) stainedGlassPlane = 1.0;

  tangent			= normalize(gl_NormalMatrix * at_tangent.xyz );
	binormal		= normalize(gl_NormalMatrix * -cross(gl_Normal, at_tangent.xyz));

  dist = length(gbufferModelView * gbufferModelViewInverse * gl_ModelViewMatrix * gl_Vertex);

	mat3 tbnMatrix = mat3(tangent.x, binormal.x, normal.x,
												tangent.y, binormal.y, normal.y,
												tangent.z, binormal.z, normal.z);

	viewVector = (gl_ModelViewMatrix * gl_Vertex).xyz;
  viewVector = (tbnMatrix * viewVector);

}
