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

varying vec2 texcoord;

varying vec3 world;

varying float id;

uniform sampler2D tex;
uniform sampler2D noisetex;

uniform float frameTimeCounter;

//#include "waterNoise.glsl"

void main() {

  gl_FragData[0] = texture2D(tex, texcoord.st);
  //if (abs(id - 8.5) < 0.6) gl_FragData[0] = vec4(vec3(mix(pow(getWaves(world.xyz), 1.5), 1.0, 0.1)) * 2.0, 1.0); //This is for shadow mappin caustics, currently disabled due to the lack of colored shadows.

}
