/*
 * (C) Copyright 2007 Adobe Systems Incorporated. All Rights Reserved.
 *
 * NOTICE:  Adobe permits you to use, modify, and distribute this file in accordance with the 
 * terms of the Adobe license agreement accompanying it.  If you have received this file from a 
 * source other than Adobe, then your use, modification, or distribution of it requires the prior 
 * written permission of Adobe. 
 * THIS CODE AND INFORMATION IS PROVIDED "AS-IS" WITHOUT WARRANTY OF
 * ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
 * PARTICULAR PURPOSE.
 *
 *  THIS CODE IS NOT SUPPORTED BY Adobe Systems Incorporated.
 *
 */

package
{
	import flash.display.Sprite;
    import flash.net.NetConnection;
    import flash.events.NetStatusEvent;
    
    public class BandwidthServer extends Sprite
    {
		private var nc:NetConnection;
		
		public function BandwidthServer() {
           nc = new NetConnection();
           nc.addEventListener(NetStatusEvent.NET_STATUS, netStatusHandler);
           nc.client = new Client();
           nc.connect("rtmp://localhost/FlashVideoApp","test1","test2");
        }
        
        private function netStatusHandler(event:NetStatusEvent):void{
            switch (event.info.code)
            {
                case "NetConnection.Connect.Success":
	                trace("The connection was made successfully");
					nc.call("checkBandwidth", null);
	                break;
                case "NetConnection.Connect.Rejected":
	                trace ("Sorry, the connection was rejected");
	                break;
	            case "NetConnection.Connect.Failed":
					trace("Failed to connect to server.");
					break;
	        }
        }

    } 
}


class Client {
    public function onBWCheck(... rest):Number {
		return 0;
	}
	
	public function onBWDone(... rest):void {
		var p_bw:Number;
		if (rest.length > 0) 
		    p_bw = rest[0];
		trace("bandwidth = " + p_bw + " Kbps.");
	} 
}