#include <erl_driver.h>
#include <openssl/sha.h>
#include <openssl/hmac.h>

/* -------------------------------------------------------------------------- */
#define DRV_HANDSHAKE 1

#ifdef DEBUG
#define DEBUGF(P) debugf P
#include <stdarg.h>
#include <stdio.h>
static void debugf(char *str, ...)
{
    va_list ap;
    va_start(ap,str);
    fprintf(stderr,"erlria_drv: ");
    vfprintf(stderr,str, ap);
    fprintf(stderr,"\r\n");
    va_end(ap);
}
#else
#define DEBUGF(P)
#endif


typedef struct erlria_data_t {
    ErlDrvPort port;
} ErlRiaData;

static int handshake(ErlRiaData* data,char *buf,int len,char **rbuf);

/* -------------------------------------------------------------------------- */
static ErlDrvData start(ErlDrvPort port, char *command){ 
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    ErlRiaData* data = (ErlRiaData *)driver_alloc(sizeof(ErlRiaData));
    return (ErlDrvData)data;
}

static void stop(ErlDrvData drv_data){
    ErlRiaData* data = (ErlRiaData *)drv_data;
    driver_free(data);
}

static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen){
    ErlRiaData* data = (ErlRiaData *)drv_data;
    int ret_len = 0;
    *rbuf = NULL;

    switch(command){
    case DRV_HANDSHAKE:
        ret_len = handshake(data,buf,len,rbuf);
        break;
    }

    return ret_len;
}

/* -------------------------------------------------------------------------- */
static const unsigned char server_key[] =
    {0x47,0x65,0x6e,0x75,0x69,0x6e,0x65,0x20,0x41,0x64,0x6f,0x62,0x65,0x20,0x46,0x6c,
     0x61,0x73,0x68,0x20,0x4d,0x65,0x64,0x69,0x61,0x20,0x53,0x65,0x72,0x76,0x65,0x72,
     0x20,0x30,0x30,0x31,0xf0,0xee,0xc2,0x4a,0x80,0x68,0xbe,0xe8,0x2e,0x00,0xd0,0xd1,
     0x02,0x9e,0x7e,0x57,0x6e,0xec,0x5d,0x2d,0x29,0x80,0x6f,0xab,0x93,0xb8,0xe6,0x36,
     0xcf,0xeb,0x31,0xae};

/* static unsigned char *server_key_hash = SHA256(server_key,sizeof(server_key),NULL); */

static int handshake(ErlRiaData* data,char *buf,int len,char **rbuf){
    ErlDrvBinary *ret_bin = NULL;
    int ret_len = 0;
    int pos = (buf[8]+buf[9]+buf[10]+buf[11]) % 728 + 12;
    const EVP_MD *evp_sha256 = EVP_sha256();
    unsigned char new_key[EVP_MAX_MD_SIZE];
    unsigned int new_key_size = EVP_MAX_MD_SIZE;
    HMAC(evp_sha256,server_key,sizeof(server_key),
         &buf[pos],32,new_key,&new_key_size);
    
    ret_bin = driver_alloc_binary(1536);
    HMAC(evp_sha256,new_key,new_key_size,
         ret_bin->orig_bytes,1504,
         &ret_bin->orig_bytes[1504],&new_key_size);
    
    /* DEBUGF(("test%d!",new_key_size)); */
    *rbuf = (char *)ret_bin;
    return ret_len;
}


/* -------------------------------------------------------------------------- */
static ErlDrvEntry erlria_driver_entry = {
    NULL,			/* init */
    start, 
    stop, 
    NULL,			/* output */
    NULL,		/* ready_input */
    NULL,			/* ready_output */ 
    "erlria_drv", 
    NULL,			/* finish */
    NULL,			/* handle */
    control, 
    NULL,			/* timeout */
    NULL,			/* outputv */
    NULL,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(erlria_drv){
    return &erlria_driver_entry;
}
