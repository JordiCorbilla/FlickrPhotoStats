using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace FlickrPhotoAnalytics.lib
{
    class Stat : IStat
    {
        public DateTime date { get; set; }
        public int views { get; set; }
        public int comments { get; set; }
        public int likes { get; set; }
    }
}