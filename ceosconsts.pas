{*****************************************************************}
{ ceosconsts is part of Ceos middleware/n-tier JSONRPC components }
{                                                                 }
{ Beta version                                                    }
{                                                                 }
{ This library is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of  }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit ceosconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  JSONRPC_VERSION         : string = '2.0';
  JSON_HEADER_CONTENT_TYPE: string = 'Content-type: application/json; charset=UTF-8';

  CEOS_VERSION            : string  = '0.0.3';
  CEOS_DEFAULT_PORT       : integer = 8080;

  ERR_INTERNAL_ERROR      = -32000;
  ERR_REQUEST_ERROR       = -32100;

  ERR_UNKNOW_FUNCTION     = -32601;
  ERR_REQUEST_CONTENT     = -32600;
  ERR_INVALID_CONTENT     = -32602;

  ERR_INVALID_QUERY       = -32300;

implementation

end.

