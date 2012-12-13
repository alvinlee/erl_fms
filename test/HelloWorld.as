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

package {
	
	import flash.display.MovieClip;
	import flash.net.Responder;
	import flash.net.NetConnection;
	import flash.events.NetStatusEvent;
	import flash.events.MouseEvent;
    
	
	public class HelloWorld extends MovieClip {
        
        // Represents a network connection.
        private var nc:NetConnection;
        
        // Responder for call to server's serverHelloMsg -- see onReply() below.
        private var myResponder:Responder = new Responder(onReply,onReply);
		
        // Constructor.
        public function HelloWorld() {
            trace(NetStatusEvent.NET_STATUS);
            connect();
        }
	
        // When button is pressed, connect to or disconnect from the server.
        public function connect():void {
            nc = new NetConnection();
            
            // Connect to the server.
            nc.connect("rtmp://localhost/HelloWorld",1,2,3);

            // Call the server's client function serverHelloMsg, in HelloWorld.asc.
            nc.call("good/serverHelloMsg", myResponder, "World", 1, 2);
            nc.call("serverHelloMsg", myResponder, "World", 1, 2);
        }
        
        // Responder function for nc.call() in connectHandler(). 
        private function onReply(result:Object):void {
            trace("onReply received value: " + result);
        }
    }
}
