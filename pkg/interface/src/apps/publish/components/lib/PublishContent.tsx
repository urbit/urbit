import React, { ReactNode } from "react";
import { Box } from "@tlon/indigo-react";
import { Link } from "react-router-dom";
import { SidebarSwitcher } from "../../../../components/SidebarSwitch";
import { useQuery } from "../../../../lib/useQuery";
import GlobalApi from "../../../../api/global";

interface PublishContentProps {
  children: ReactNode;
  sidebarShown: boolean;
  api: GlobalApi;
}

export function PublishContent(props: PublishContentProps) {
  const { children, sidebarShown, api } = props;

  const { query } = useQuery();
  const popout = !!query.get("popout");

  const popoutDisplay = popout ? [] : ["none", "block"];

  return (
    <Box
      py={2}
      px={3}
      fontSize={0}
      height="100%"
      width="100%"
      display="grid"
      gridTemplateColumns="16px 1fr 16px"
      gridTemplateRows="1fr"
    >
      <SidebarSwitcher popout={popout} sidebarShown={sidebarShown} api={api} />
      {children}
      <Box pt={2} justifySelf="end" display={popoutDisplay}>
        <Link target="_blank" to="">
          <img src="/~landscape/img/popout.png" height={16} width={16} />
        </Link>
      </Box>
    </Box>
  );
}
