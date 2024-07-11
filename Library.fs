//
// F# image processing functions.
//
// More details?
//
// Ahmed Shah. 
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let MakeGray (r,g,b) = ( int( 0.299 * float(r) + 0.587 * float(g) + 0.114 * float(b)), int( 0.299 * float(r) + 0.587 * float(g) + 0.114 * float(b)), int( 0.299 * float(r) + 0.587 * float(g) + 0.114 * float(b)) )
  
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:

    let grayScImage =  List.map(fun x -> List.map MakeGray x) image

    grayScImage


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let ChangeThresholdRGB rgb threshold depth =
    match rgb with
    | rgb when rgb <= threshold -> 0
    | rgb when rgb > threshold -> depth

    
  
  let ThresholdChange (r,g,b) threshold depth = ( int(ChangeThresholdRGB r threshold depth), int(ChangeThresholdRGB g threshold depth), int(ChangeThresholdRGB b threshold depth) ) 
  
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    let ThresholdChange (r,g,b) = ( int(ChangeThresholdRGB r threshold depth), int(ChangeThresholdRGB g threshold depth), int(ChangeThresholdRGB b threshold depth) ) 
    
    let thresholdImage = List.map(fun x -> List.map ThresholdChange x) image 
    thresholdImage


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    let flipImage = List.map(List.rev) image
    flipImage


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let DiffCalc pi r b thresh depth =
    let (r1,g1,b1) = pi 
    let (r2,g2,b2) = r 
    let (r3,g3,b3) = b  

    let dist1 = sqrt( float( (r1-r2)*(r1-r2) + (g1-g2)*(g1-g2) + (b1-b2)*(b1-b2) ) )
    let dist2 = sqrt( float( (r1-r3)*(r1-r3) + (g1-g3)*(g1-g3) + (b1-b3)*(b1-b3) ) )

    if (dist1 > float(thresh))  then 
      (0,0,0)
    elif (dist2 > float(thresh)) then 
      (0,0,0)
    else 
      (depth,depth,depth)
  
  

  let rec HelpEdge h1 h2 thresh depth = 
    match h1 with 
    | [] -> []
    | head::[] -> []
    | head::tail -> (DiffCalc head (List.head tail) (List.head h2) thresh depth) :: (HelpEdge tail (List.tail h2) thresh depth)
  
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    match image with
    | [] -> []
    | head::[] -> []
    | head::tail -> (HelpEdge head (List.head tail) threshold depth)::(EdgeDetect width height depth tail threshold)


  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    //let rotatedImage = List.transpose List.map(List.rev) image
    //let rotatedImage = List.map( List.rev (List.transpose)) image
    let rotatedImage = image |> List.transpose |> List.map(List.rev )
    
    //let varHeight = height 
    //height <- width 
    //width <-  varHeight 
    rotatedImage

