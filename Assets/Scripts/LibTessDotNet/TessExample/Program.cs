using System;
//using System.Drawing;
using UnityEngine;

namespace TessExample
{
    class Program
    {
        // The data array contains 4 values, it's the associated data of the vertices that resulted in an intersection.
        private static object VertexCombine(LibTessDotNet.Vec3 position, object[] data, float[] weights)
        {
            // Fetch the vertex data.
            var colors = new Color[] { (Color)data[0], (Color)data[1], (Color)data[2], (Color)data[3] };
            // Interpolate with the 4 weights.
            var rgba = new float[] {
                colors[0].r * weights[0] + colors[1].r * weights[1] + colors[2].r * weights[2] + colors[3].r * weights[3],
                colors[0].g * weights[0] + colors[1].g * weights[1] + colors[2].g * weights[2] + colors[3].g * weights[3],
                colors[0].b * weights[0] + colors[1].b * weights[1] + colors[2].b * weights[2] + colors[3].b * weights[3],
                colors[0].a * weights[0] + colors[1].a * weights[1] + colors[2].a * weights[2] + colors[3].a * weights[3]
            };
            // Return interpolated data for the new vertex.
            return new Color(rgba[0], rgba[1], rgba[2], rgba[3]);
        }

        public static LibTessDotNet.Tess tessDemo()
        {
            // Example input data in the form of a star that intersects itself.
            var inputData = new float[] { 0.0f, 3.0f, -1.0f, 0.0f, 1.6f, 1.9f, -1.6f, 1.9f, 1.0f, 0.0f };

            // Create an instance of the tessellator. Can be reused.
            var tess = new LibTessDotNet.Tess();

            // Construct the contour from inputData.
            // A polygon can be composed of multiple contours which are all tessellated at the same time.
            int numPoints = inputData.Length / 2;
            var contour = new LibTessDotNet.ContourVertex[numPoints];
            for (int i = 0; i < numPoints; i++)
            {
                // NOTE : Z is here for convenience if you want to keep a 3D vertex position throughout the tessellation process but only X and Y are important.
                contour[i].Position = new LibTessDotNet.Vec3 { X = inputData[i * 2], Y = inputData[i * 2 + 1], Z = 0.0f };
                // Data can contain any per-vertex data, here a constant color.
                contour[i].Data = Color.blue;
            }
            // Add the contour with a specific orientation, use "Original" if you want to keep the input orientation.
            tess.AddContour(contour, LibTessDotNet.ContourOrientation.Clockwise);

            // Tessellate!
            // The winding rule determines how the different contours are combined together.
            // See http://www.glprogramming.com/red/chapter11.html (section "Winding Numbers and Winding Rules") for more information.
            // If you want triangles as output, you need to use "Polygons" type as output and 3 vertices per polygon.
            tess.Tessellate(LibTessDotNet.WindingRule.EvenOdd, LibTessDotNet.ElementType.Polygons, 3, VertexCombine);

            // Same call but the last callback is optional. Data will be null because no interpolated data would have been generated.
            //tess.Tessellate(LibTessDotNet.WindingRule.EvenOdd, LibTessDotNet.ElementType.Polygons, 3); // Some vertices will have null Data in this case.

            Debug.Log("Output triangles:");
            int numTriangles = tess.ElementCount;
            //for (int i = 0; i < numTriangles; i++)
            //{
            //    var v0 = tess.Vertices[tess.Elements[i * 3]].Position;
            //    var v1 = tess.Vertices[tess.Elements[i * 3 + 1]].Position;
            //    var v2 = tess.Vertices[tess.Elements[i * 3 + 2]].Position;
            //    Debug.Log("#{0} ({1:F1},{2:F1}) ({3:F1},{4:F1}) ({5:F1},{6:F1})", i, v0.X, v0.Y, v1.X, v1.Y, v2.X, v2.Y);
            //}
            //Console.ReadLine();
            return tess;
        }
    }
}
