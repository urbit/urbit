import React, {ReactNode} from "react";
import moment from "moment";
import { Sigil } from "~/logic/lib/sigil"
import { uxToHex, cite } from "~/logic/lib/util";
import { Contacts } from "~/types/contact-update";
import { Row, Box } from "@tlon/indigo-react";

interface AuthorProps {
  contacts: Contacts;
  ship: string;
  date: number;
  showImage?: boolean;
  hideAvatars: boolean;
  hideNicknames: boolean;
  children?: ReactNode;
}

export function Author(props: AuthorProps) {
  const { contacts, ship = '', date, showImage } = props;
  let contact = null;
  if (contacts) {
    contact = ship in contacts ? contacts[ship] : null;
  }
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : "#000000";
  const showAvatar = !props.hideAvatars && contact?.avatar;
  const showNickname = !props.hideNicknames && contact?.nickname;

  const name = showNickname ? contact?.nickname : cite(ship);
  const dateFmt = moment(date).fromNow();
  return (
    <Row alignItems="center" width="auto">
      {showImage && (
        <Box>
          {showAvatar ? (
            <img src={contact?.avatar} height={16} width={16} className="dib" />
          ) : (
            <Sigil
              ship={ship}
              size={16}
              icon
              padded
              color={color}
              classes={contact?.color ? '' : "mix-blend-diff"}
            />
          )}
        </Box>
      )}
      <Box
        ml={showImage ? 2 : 0}
        color="black"
        lineHeight='tall'
        fontFamily={showNickname ? "sans" : "mono"}
        fontWeight={showNickname ? '500' : '400'}
      >
        {name}
      </Box>
      <Box ml={2} color="gray">
        {dateFmt}
      </Box>
      {props.children}
    </Row>
  );
}
