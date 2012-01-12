uniform sampler2D uSampler;
uniform float     uWidth;
uniform float     uHeight;

void main()
{
	vec2 theTexCoord = vec2( gl_FragCoord.x/uWidth, gl_FragCoord.y/uHeight );
	vec4 theColor = texture2D( uSampler, theTexCoord );
	gl_FragColor = theColor;
}
