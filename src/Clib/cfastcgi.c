/* ;;;; Copyright(c) 2010 Joseph Donaldson(donaldsonjw@yahoo.com)  */
/* ;;;; This file is part of Fastcgi. */
/* ;;;; */
/* ;;;;     Fastcgi is free software: you can redistribute it and/or modify */
/* ;;;;     it under the terms of the GNU Lesser General Public License as */
/* ;;;;     published by the Free Software Foundation, either version 3 of the */
/* ;;;;     License, or (at your option) any later version. */
/* ;;;; */
/* ;;;;     Fastcgi is distributed in the hope that it will be useful, but */
/* ;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of */
/* ;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU */
/* ;;;;     Lesser General Public License for more details. */
/* ;;;; */
/* ;;;;     You should have received a copy of the GNU Lesser General Public */
/* ;;;;     License along with Fastcgi.  If not, see */
/* ;;;;     <http://www.gnu.org/licenses/>. */

#include "bigloo.h"
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>

#include "cfastcgi.h"

#define FCGI_LISTENSOCK_FILENO 0

obj_t get_fastcgi_socket(void){
  obj_t a_socket = BNIL;
  
  struct sockaddr_storage addr = {0};
  socklen_t addr_length = sizeof(addr);
  unsigned short port = 0;
  unsigned short family = PF_UNSPEC;

  getsockname(FCGI_LISTENSOCK_FILENO, (struct sockaddr*)&addr, &addr_length);
  
  family = ((struct sockaddr*)&addr)->sa_family;

  if(PF_INET  == family){
    port = ((struct sockaddr_in*)&addr)->sin_port;
  }
  

  a_socket = GC_MALLOC( SOCKET_SIZE );
  a_socket->socket.header = MAKE_HEADER( SOCKET_TYPE, 0 );
  a_socket->socket.portnum = ntohs( port );
  a_socket->socket.hostname = BUNSPEC;
  a_socket->socket.hostip = BFALSE;
  a_socket->socket.fd = FCGI_LISTENSOCK_FILENO;
  a_socket->socket.input = BFALSE;
  a_socket->socket.output = BFALSE;
  a_socket->socket.stype = BGL_SOCKET_SERVER;
  a_socket->socket.accept = 0L;
  a_socket->socket.userdata = BUNSPEC;
  
  return BREF( a_socket );
  
}
