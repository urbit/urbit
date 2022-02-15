import React, { MouseEvent, useMemo, useState } from 'react';
import { Link, useHistory } from 'react-router-dom';
import { Box, Icon, Image, Row } from '@tlon/indigo-react';
import UqbarLogo from '~/assets/img/uqbar-logo.png';
import useLocalState, { selectLocalState } from '~/logic/state/local';
import useHarkState from '~/logic/state/hark';
import { useDmUnreads } from '~/logic/lib/useDmUnreads';

const localSel = selectLocalState(['toggleOmnibox']);

type NavItemIcon = 'Home' | 'Messages' | 'Notifications' | 'Menu';

interface MobileNavItemProps {
  icon: NavItemIcon;
  selected?: boolean;
  notifications?: number;
}

interface NavItemLinkProps extends MobileNavItemProps {
  to: string;
}

function MobileNavItem({
  icon,
  notifications,
  selected = false
}: MobileNavItemProps) {
  return (
    <Box p={2}
      width="25vw"
      display="flex"
      flexDirection="column"
      alignItems="center"
      position="relative"
    >
      {icon === 'Home' ? (
        <Image
          referrerPolicy="no-referrer"
          src={UqbarLogo}
          height="24px"
          width="24px"
          onError={console.warn}
        />
      ) : (
        <Icon size={24} icon={icon} borderBottom={selected ? '2px solid lightGray' : undefined} />
      )}
      {Boolean(notifications) && (
        <Box
          position="absolute" right="calc(50% - 18px)"
          top="4px" px="4px"
          py="1px"
          borderRadius="8px" backgroundColor="blue"
          fontSize="12px" color="white"
        >
          {notifications}
        </Box>
      )}
    </Box>
  );
}

export function MobileNavbar() {
  const history = useHistory();
  const { toggleOmnibox } = useLocalState(localSel);
  const { notificationsCount } = useHarkState();
  const { unreadDmCount } = useDmUnreads();
  const [currentPathname, setCurrentPathname] = useState(history.location.pathname);

  const options: NavItemLinkProps[] = useMemo(() => [
    {
      icon: 'Home',
      to: '/'
    },
    {
      icon: 'Messages',
      to: '/~landscape/messages'
    },
    {
      icon: 'Notifications',
      to: '/~notifications'
    },
    {
      icon: 'Menu',
      to: '/'
    }
  ], []);

  const onClick = (icon: NavItemIcon, to: string) => (e: MouseEvent) => {
    if (icon === 'Menu') {
      e.preventDefault();
      e.stopPropagation();
      toggleOmnibox();
    } else {
      setCurrentPathname(to);
    }
  };

  const getNotifications = (icon: string) : number => (icon === 'Notifications' && Math.max(0, notificationsCount - unreadDmCount))
    || (icon === 'Messages' && unreadDmCount)
    || 0;

  return (
    <Row backgroundColor="washedGray" height="40px">
      {options.map(({ icon, to }) => <Link key={icon} to={to} onClick={onClick(icon, to)}>
        <MobileNavItem icon={icon}
          selected={currentPathname.includes(to) && icon !== 'Menu'}
          notifications={getNotifications(icon)}
        />
      </Link>)}
    </Row>
  );
}
