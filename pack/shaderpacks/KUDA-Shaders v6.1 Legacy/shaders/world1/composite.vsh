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

varying vec3 lightVector;
varying vec2 texcoord;

varying vec3 ambientColor;
varying vec3 underwaterColor;
varying vec3 torchColor;
varying vec3 waterColor;
varying vec3 lowlightColor;

uniform sampler2D noisetex;

uniform vec3 sunPosition;
uniform vec3 moonPosition;

uniform float rainStrength;
uniform float frameTimeCounter;

uniform int worldTime;


void main() {

  texcoord = gl_MultiTexCoord0.st;

	gl_Position = ftransform();

  if (float(worldTime) < 12700 || float(worldTime) > 23250) {
		lightVector = normalize(sunPosition);
	} else {
		lightVector = normalize(moonPosition);
	}

  ambientColor  = vec3(0.9, 0.8, 1.0);

	underwaterColor = vec3(0.1, 0.75, 1.0) * 0.1;

	torchColor = vec3(1.0, 0.64, 0.4);

  waterColor = vec3(0.7, 0.9, 1.0);

	lowlightColor = vec3(0.65, 0.8, 1.0);

}
