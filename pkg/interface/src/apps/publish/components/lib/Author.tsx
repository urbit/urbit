import React from "react";
import moment from "moment";
import { Sigil } from "../../../../lib/sigil";
import { uxToHex, cite } from "../../../../lib/util";
import { Contacts } from "../../../../types/contact-update";
import { Row, Box } from "@tlon/indigo-react";

interface AuthorProps {
  contacts: Contacts;
  ship: string;
  date: number;
  showImage?: boolean;
}

export function Author(props: AuthorProps) {
  const { contacts, ship, date, showImage } = props;
  const noSig = ship.slice(1);
  const contact = noSig in contacts ? contacts[noSig] : null;
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : "#000000";
  const name = contact?.nickname || cite(ship);

  const dateFmt = moment(date).fromNow();
  return (
    <Row alignItems="center" width="auto">
      {showImage && (
        <Box>
          {contact?.avatar ? (
            <img src={contact?.avatar} height={24} width={24} className="dib" />
          ) : (
            <Sigil
              ship={ship}
              size={24}
              color={color}
              classes="mix-blend-diff"
            />
          )}
        </Box>
      )}
      <Box
        ml={showImage ? 2 : 0}
        color="gray"
        fontFamily={contact?.nickname ? "sans" : "mono"}
      >
        {name}
      </Box>
      <Box ml={2} color="gray">
        {dateFmt}
      </Box>
    </Row>
  );
}
