uniform sampler2D uSampler;
uniform float     uWidth;
uniform float     uHeight;

void main (void)
{
	float uOneXPixel = 1.0/uWidth;
	float uOneYPixel = 1.0/uHeight;
	vec2 theTexCoord = vec2( gl_FragCoord.x/uWidth, gl_FragCoord.y/uHeight );

	int neighbors = 0;
	if ( texture2D( uSampler, vec2( theTexCoord.x - uOneXPixel, theTexCoord.y - uOneYPixel ) ).r > 0.0 ) neighbors = neighbors + 1;
	if ( texture2D( uSampler, vec2( theTexCoord.x             , theTexCoord.y - uOneYPixel ) ).r > 0.0 ) neighbors = neighbors + 1;
	if ( texture2D( uSampler, vec2( theTexCoord.x + uOneXPixel, theTexCoord.y - uOneYPixel ) ).r > 0.0 ) neighbors = neighbors + 1;

	if ( texture2D( uSampler, vec2( theTexCoord.x - uOneXPixel, theTexCoord.y              ) ).r > 0.0 ) neighbors = neighbors + 1;
	if ( texture2D( uSampler, vec2( theTexCoord.x + uOneXPixel, theTexCoord.y              ) ).r > 0.0 ) neighbors = neighbors + 1;

	if ( texture2D( uSampler, vec2( theTexCoord.x - uOneXPixel, theTexCoord.y + uOneYPixel ) ).r > 0.0 ) neighbors = neighbors + 1;
	if ( texture2D( uSampler, vec2( theTexCoord.x             , theTexCoord.y + uOneYPixel ) ).r > 0.0 ) neighbors = neighbors + 1;
	if ( texture2D( uSampler, vec2( theTexCoord.x + uOneXPixel, theTexCoord.y + uOneYPixel ) ).r > 0.0 ) neighbors = neighbors + 1;

	bool theLiveFlag = texture2D( uSampler, theTexCoord ).r > 0.0;
	if ( theLiveFlag )
	{
		if ( neighbors < 2 || neighbors > 3 )
			gl_FragColor = vec4(0,0,0,1);
		else
			gl_FragColor = vec4(1,1,1,1);
	}
	else // You're dead
	{
		if ( neighbors == 3 )
			gl_FragColor = vec4(1,1,1,1);
		else
			gl_FragColor = vec4(0,0,0,1);
	}
}
