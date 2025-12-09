#pragma once

#include <cstdio>
#include <vector>

using namespace std;

struct Color {
    unsigned char R;
    unsigned char G;
    unsigned char B;
};

void save_bitmap(
    const char* filename,
    const vector<vector<Color>>& image
) {
    if (image.empty() || image[0].empty()) return;

    int height = image.size();
    int width = image[0].size();

    FILE* f = fopen(filename, "wb");
    if (!f) return;

    unsigned char fileHeader[14] = {
        'B', 'M',   // Signature
        0, 0, 0, 0, // File size (filled later)
        0, 0,       // Reserved1
        0, 0,       // Reserved2
        54, 0, 0, 0 // Data offset
    };

    unsigned char infoHeader[40] = {
        40, 0, 0, 0, // Size of info header
        0, 0, 0, 0,  // Width (filled later)
        0, 0, 0, 0,  // Height (filled later)
        1, 0,        // Planes
        24, 0,       // Bit count
        0, 0, 0, 0,  // Compression
        0, 0, 0, 0,  // Image size (filled later)
        0, 0, 0, 0,  // X pixels per meter
        0, 0, 0, 0,  // Y pixels per meter
        0, 0, 0, 0,  // Colors used
        0, 0, 0, 0   // Colors important
    };

    int paddingSize = (4 - (width * 3) % 4) % 4;
    int fileSize = 54 + (width * 3 + paddingSize) * height;

    fileHeader[2] = (unsigned char)(fileSize);
    fileHeader[3] = (unsigned char)(fileSize >> 8);
    fileHeader[4] = (unsigned char)(fileSize >> 16);
    fileHeader[5] = (unsigned char)(fileSize >> 24);

    infoHeader[4] = (unsigned char)(width);
    infoHeader[5] = (unsigned char)(width >> 8);
    infoHeader[6] = (unsigned char)(width >> 16);
    infoHeader[7] = (unsigned char)(width >> 24);

    infoHeader[8] = (unsigned char)(height);
    infoHeader[9] = (unsigned char)(height >> 8);
    infoHeader[10] = (unsigned char)(height >> 16);
    infoHeader[11] = (unsigned char)(height >> 24);

    fwrite(fileHeader, 1, 14, f);
    fwrite(infoHeader, 1, 40, f);

    unsigned char padding[3] = {0, 0, 0};

    for (int y = height - 1; y >= 0; y--) {
        for (int x = 0; x < width; x++) {
            unsigned char color[3] = {
                image[y][x].B,
                image[y][x].G,
                image[y][x].R
            };
            fwrite(color, 1, 3, f);
        }
        fwrite(padding, 1, paddingSize, f);
    }

    fclose(f);
}