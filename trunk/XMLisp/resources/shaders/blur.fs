#version 120

uniform sampler2D tex;    // main texture
uniform float offset;     // texture coordinate offset [0.0 .. 1.0]
uniform float saturation; // color saturation: 0.0 = gray scale, 1.0 = regular color, > 1.0 oversaturated 


void main()
{

	vec4 Color = 0.25 * (texture2D(tex, gl_TexCoord[0].xy + vec2(offset, offset)) + 
					   texture2D(tex, gl_TexCoord[0].xy + vec2(offset, -offset)) + 
                          texture2D(tex, gl_TexCoord[0].xy + vec2(-offset, offset)) +
                          texture2D(tex, gl_TexCoord[0].xy + vec2(-offset, -offset)));

	float grayValue = (Color.r + Color.g + Color.b) / 3.0;

	gl_FragColor = saturation * Color + (1.0 - saturation) * vec4(grayValue, grayValue, grayValue, Color.a);
}
