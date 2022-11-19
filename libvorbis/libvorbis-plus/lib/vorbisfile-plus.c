#include "vorbis/vorbisfile.h"

void ov_callbacks_default_ptr (ov_callbacks *callbacks)
  {
    *callbacks = OV_CALLBACKS_DEFAULT;
  }

void ov_callbacks_noclose_ptr (ov_callbacks *callbacks)
  {
    *callbacks = OV_CALLBACKS_NOCLOSE;
  }

void ov_callbacks_streamonly_ptr (ov_callbacks *callbacks)
  {
    *callbacks = OV_CALLBACKS_STREAMONLY;
  }

void ov_callbacks_streamonly_noclose_ptr (ov_callbacks *callbacks)
  {
    *callbacks = OV_CALLBACKS_STREAMONLY_NOCLOSE;
  }

int ov_open_callbacks_plus
      ( void *f
      , OggVorbis_File *vf
      , const char *initial
      , long ibytes
      , ov_callbacks *callbacks
      )
  {
    ov_open_callbacks (f, vf, initial, ibytes, *callbacks);
  }

int ov_test_callbacks_plus
      ( void *f
      , OggVorbis_File *vf
      , const char *initial
      , long ibytes
      , ov_callbacks *callbacks
      )
  {
    ov_test_callbacks (f, vf, initial, ibytes, *callbacks);
  }
