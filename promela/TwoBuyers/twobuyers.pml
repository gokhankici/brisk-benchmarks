/** Two Buyers protocol
*/
#include "../Common/defs.pml"

mtype = { Title, Quote, BuyerQuote, OK, Quit, OfferDetails, SD }
typedef TitleMsg { mtype tag; pid p; pid q; int s; };
typedef QuoteMsg { mtype tag; pid p; int i; };
typedef BuyerQuoteMsg { mtype tag; pid p; int i; };
typedef OfferMsg { mtype tag; pid p; };
typedef OfferDetailsMsg { mtype tag; pid p; int s; };
typedef ShippingDateMsg { mtype tag; int d; int m; int y; };

DECLARE_CHAN(buyerone_seller,1,TitleMsg);
DECLARE_CHAN(buyerone_buyertwo,1,BuyerQuoteMsg);
DECLARE_CHAN(buyertwo_seller,1,OfferMsg);
DECLARE_CHAN(buyertwo_seller,1,OfferDetailsMsg);
DECLARE_CHAN(seller_buyerone,1,QuoteMsg);
DECLARE_CHAN(seller_buyertwo,1,QuoteMsg);
DECLARE_CHAN(seller_buyertwo,1,ShippingDateMsg);

proctype buyerone(int off)
{
  pid me = _pid - off;
  TitleMsg titlemsg;
  QuoteMsg quotemsg;
  BuyerQuoteMsg buyerquotemsg;

  titlemsg.tag = Title;
  titlemsg.p = me;
  titlemsg.q = 3;
  CHAN(buyerone_seller,TitleMsg)[0]!titlemsg;

  CHAN(seller_buyerone,QuoteMsg)[0]?quotemsg;

  buyerquotemsg.p = me;
  buyerquotemsg.i = quotemsg.i / 2;
  CHAN(buyerone_buyertwo,BuyerQuoteMsg)[0]!buyerquotemsg;
}

proctype buyertwo(int off)
{
  pid me = _pid - off;
  QuoteMsg quotemsg;
  BuyerQuoteMsg buyerquotemsg;
  OfferMsg offer;
  OfferDetailsMsg odmsg;
  ShippingDateMsg sdmsg;
  bool buy;

  CHAN(seller_buyertwo,QuoteMsg)[0]?quotemsg;
  CHAN(buyerone_buyertwo,BuyerQuoteMsg)[0]?buyerquotemsg;

  if :: buy = true; :: buy = false fi;

  if
    :: buy ->
       offer.tag = OK;
       offer.p   = me;
       CHAN(buyertwo_seller,OfferMsg)[0]!offer;
       CHAN(buyertwo_seller,OfferDetailsMsg)[0]!odmsg;
       CHAN(seller_buyertwo,ShippingDateMsg)[0]?sdmsg;
    :: !buy ->
       offer.tag = Quit;
       offer.p   = me;
       CHAN(buyertwo_seller,OfferMsg)[0]!offer;
  fi
}

proctype seller(int off)
{
  pid me = _pid - off;
  TitleMsg title;
  QuoteMsg quote;
  OfferMsg offer;
  OfferDetailsMsg odmsg;
  ShippingDateMsg sdmsg;
 
  CHAN(buyerone_seller,TitleMsg)[0]?title;

  quote.tag = Quote;
  quote.p   = me;
  quote.i   = 2;
  CHAN(seller_buyerone,QuoteMsg)[0]!quote;
  CHAN(seller_buyertwo,QuoteMsg)[0]!quote;

  CHAN(buyertwo_seller,OfferMsg)[0]?offer;
  if
    :: offer.tag == Quit -> skip;
    :: offer.tag == OK ->
       CHAN(buyertwo_seller,OfferDetailsMsg)[0]?odmsg;
       sdmsg.tag = SD;
       sdmsg.d = 1;
       sdmsg.m = 1;
       sdmsg.y = 1970;
       CHAN(seller_buyertwo,ShippingDateMsg)[0]!sdmsg;
  fi
}

init {
  int x = _nr_pr;
  run seller(x);
  x = _nr_pr;
  run buyerone(x);
  x = _nr_pr;
  run buyertwo(x);
}