#version 330

uniform sampler2D previous;
uniform ivec2 resolution;
uniform vec2 pixel_size;
in vec2 uv;
layout (location = 0) out vec4 color;

const bool born[] = bool[]( false
                          , false
                          , false
                          , true
                          , false
                          , false
                          , true
                          , true
                          , true );
const bool alive[] = bool[]( false
                           , false
                           , false
                           , true
                           , true
                           , false
                           , true
                           , true
                           , true );

vec4 neighs[8];

int neighbour_contribution(vec4 col)
{
    float avg = (col.r + col.b) * 0.5;
    if ( avg > 0.5 ) {
        return 1;
    }
    return 0;
}

void main() {
    neighs[0] = texture(previous, uv+vec2(pixel_size.x, 0));
    neighs[1] = texture(previous, uv-vec2(pixel_size.x, 0));
    neighs[2] = texture(previous, uv+vec2(0, pixel_size.y));
    neighs[3] = texture(previous, uv-vec2(0, pixel_size.y));
    neighs[4] = texture(previous, uv+vec2(pixel_size.x, pixel_size.y));
    neighs[5] = texture(previous, uv+vec2(-pixel_size.x, pixel_size.y));
    neighs[6] = texture(previous, uv+vec2(pixel_size.x, -pixel_size.y));
    neighs[7] = texture(previous, uv+vec2(-pixel_size.x, -pixel_size.y));

    int num_neighbours = 0;
    int reds = 0;
    int blues = 0;
    int greens = 0;
    for ( int i = 0; i < 8; ++i ) {
        neighs[i].g = 0;
        int c = neighbour_contribution(neighs[i]);
        num_neighbours += c;
        if ( c > 0 ) {
            if ( neighs[i].r > neighs[i].b &&
                 neighs[i].r > neighs[i].g ) {
                reds++;
            } else if ( neighs[i].b > neighs[i].r &&
                        neighs[i].b > neighs[i].g ) {
                blues++;
            } else {
                greens++;
            }
        }
    }

    int is_alive = neighbour_contribution(texture(previous, uv));

    // Conway rule
    if ( is_alive == 1 && alive[num_neighbours] ) {
        color = texture(previous, uv);
    } else if ( is_alive == 0 && born[num_neighbours] ) {
        if ( reds > blues && reds > greens ) {
            color = vec4( 3, 0, 0, 1 );
        } else if ( blues > reds && blues > greens ) {
            color = vec4( 0, 0, 3, 1 );
        } else {
            color = vec4( 0, 3, 0, 1 );
        }
    } else {
        color = vec4( 0, 0, 0, 1 );
    }
}

