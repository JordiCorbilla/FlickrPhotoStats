using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace FlickrPhotoAnalytics.lib
{
    interface IStat
    {
        DateTime date { get; set; }
        int views { get; set; }
        int comments { get; set; }
        int likes { get; set; }
    }
}