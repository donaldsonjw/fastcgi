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
  a_socket->socket_t.header = MAKE_HEADER( SOCKET_TYPE, 0 );
  a_socket->socket_t.portnum = ntohs( port );
  a_socket->socket_t.hostname = BUNSPEC;
  a_socket->socket_t.hostip = BFALSE;
  a_socket->socket_t.fd = FCGI_LISTENSOCK_FILENO;
  a_socket->socket_t.input = BFALSE;
  a_socket->socket_t.output = BFALSE;
  a_socket->socket_t.stype = BGL_SOCKET_SERVER;
  a_socket->socket_t.accept = 0L;
  a_socket->socket_t.userdata = BUNSPEC;
  
  return BREF( a_socket );
  
}
