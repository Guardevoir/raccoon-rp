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
varying vec2 texcoord;
varying vec2 lmcoord;

uniform mat4 gbufferModelView;
uniform mat4 gbufferModelViewInverse;

uniform vec3 cameraPosition;

uniform float frameTimeCounter;
uniform int worldTime;

#ifdef animateUsingWorldTime
	#define frameTimeCounter worldTime * 0.0416
#endif

void main() {

  lmcoord  = (gl_TextureMatrix[1] * gl_MultiTexCoord1).xy;
  texcoord = gl_MultiTexCoord0.st;
  color    = gl_Color;

	vec4 position = gbufferModelViewInverse * gl_ModelViewMatrix * gl_Vertex;
  vec3 worldposition = position.xyz + cameraPosition.xyz;

  #ifdef shakingCamera
		position += vec4(0.02 * sin(frameTimeCounter * 2.0), 0.005 * cos(frameTimeCounter * 3.0), 0.0, 0.0) * gbufferModelView;
	#endif

  bool istopv = worldposition.y > cameraPosition.y + 5.0;

	if (!istopv) position.xz += vec2(1.0, 0.0);

	gl_Position = gl_ProjectionMatrix * gbufferModelView * position;

}
