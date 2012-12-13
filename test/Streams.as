package {
    import flash.display.Sprite;
    import flash.net.NetConnection;
    import flash.events.NetStatusEvent;
    import flash.events.AsyncErrorEvent;
    
    import flash.net.NetStream;
    import flash.media.Video;
    import flash.media.Microphone;
    import flash.media.Camera;

    public class Streams extends Sprite
    {
        var nc:NetConnection;
        var stream:NetStream;
		var playStream:NetStream;
        var video:Video;
        
        public function Streams()
        {
            nc = new NetConnection();
            nc.addEventListener(NetStatusEvent.NET_STATUS, netStatusHandler);
            nc.client = new CustomClient();
			nc.connect("rtmp://localhost/Streams");
			// nc.connect("rtmp://localhost/vod");
        }
        
        private function netStatusHandler(event:NetStatusEvent):void
        {
            trace("connected is: " + nc.connected );
			trace("event.info.level: " + event.info.level);
			trace("event.info.code: " + event.info.code);
			
            switch (event.info.code)
                {
                case "NetConnection.Connect.Success":
	                trace("Congratulations! you're connected");
	                connectStream(nc);
					// createPlayList(nc);
	                // instead you can also call createPlayList() here
	                break;
				case "NetConnection.Connect.Failed":
                case "NetConnection.Connect.Rejected":
	                trace ("Oops! the connection was rejected");
	                break;
	            case "NetStream.Play.Stop":
	                trace("The stream has finished playing");
	                break;
	            case "NetStream.Play.StreamNotFound":
	                trace("The server could not find the stream you specified"); 
	                break;
	            case "NetStream.Publish.BadName":
	                trace("The stream name is already used");
	                break;
                }
        }
        
        
        // play a recorded stream on the server
        private function connectStream(nc:NetConnection):void {
            stream = new NetStream(nc);
            stream.addEventListener(NetStatusEvent.NET_STATUS, netStatusHandler);
            stream.client = new CustomClient();
            
            video = new Video();
            video.attachNetStream(stream);
            
            stream.play("mp4:test", 0);
            addChild(video);
        }        
        
        // create a playlist on the server
        private function createPlayList(nc:NetConnection):void {
            stream = new NetStream(nc);
            stream.addEventListener(NetStatusEvent.NET_STATUS, netStatusHandler);
            stream.client = new CustomClient();
            
            video = new Video();
            video.attachNetStream(stream);
            
            // play the first 10 seconds of bikes
            stream.play( "bikes", 0, 10, true );
			// then from 20 seconds onwards
            stream.play( "bikes", 20, -1, false);
			// finally just the frame at 5 seconds to end on
			stream.play( "bikes", 5, 0, false );
            addChild(video);
        }      
    }   
}

class CustomClient {
    public function onMetaData(info:Object):void {
        trace("metadata: duration=" + info.duration + " width=" + info.width + " height=" + info.height + " framerate=" + info.framerate);
    }
    public function onPlayStatus(info:Object):void {
        trace("handling playstatus here");
    }

     public function onBWCheck(... rest):Number {
		    return 0;
     }
		
	 public function onBWDone(... rest):void {
		    var p_bw:Number;
		    if (rest.length > 0) p_bw = rest[0];
		    // do something here
		    // when the bandwidth check is complete
		    trace("bandwidth = " + p_bw + " Kbps.");
	}
}
