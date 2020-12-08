import React, {ReactNode} from "react";
import moment from "moment";
import { Row, Box } from "@tlon/indigo-react";

import { Sigil } from "~/logic/lib/sigil"
import { uxToHex, cite } from "~/logic/lib/util";
import { Contacts, Rolodex } from "~/types/contact-update";
import OverlaySigil from "./OverlaySigil";
import { Group, Association } from "~/types";
import GlobalApi from "~/logic/api/global";
import { useHistory } from "react-router-dom";

interface AuthorProps {
  contacts: Contacts;
  ship: string;
  date: number;
  showImage?: boolean;
  hideAvatars: boolean;
  hideNicknames: boolean;
  children?: ReactNode;
  group: Group;
  api: GlobalApi;
}

export default function Author(props: AuthorProps) {
  const { contacts, ship = '', date, showImage, hideAvatars, hideNicknames, group, api } = props;
  const history = useHistory();
  let contact;
  if (contacts) {
    contact = ship in contacts ? contacts[ship] : null;
  }
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : "#000000";
  const showNickname = !props.hideNicknames && contact?.nickname;

  const name = showNickname ? contact?.nickname : cite(ship);
  const dateFmt = moment(date).fromNow();
  return (
    <Row alignItems="center" width="auto">
      {showImage && (
        <Box>
          <OverlaySigil
          ship={ship}
          contact={contact}
          color={color}
          sigilClass={''}
          group={group}
          hideAvatars={hideAvatars}
          hideNicknames={hideNicknames}
          history={history}
          api={api}
          bg="white"
          className="fl v-top pt1"
        />
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
