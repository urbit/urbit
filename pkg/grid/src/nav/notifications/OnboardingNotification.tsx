import React from 'react';
import cn from 'classnames';
import { Link } from 'react-router-dom';
import { HarkLid, Vats, getVatPublisher } from '@urbit/api';
import { Button } from '../../components/Button';
import { useBrowserId, useCurrentTheme } from '../../state/local';
import { getDarkColor } from '../../state/util';
import useKilnState from '../../state/kiln';
import { useHarkStore } from '../../state/hark';
import { useProtocolHandling } from '../../state/settings';

const getCards = (vats: Vats, protocol: boolean): OnboardingCardProps[] => {
  const cards = [
    {
      title: 'Terminal',
      body: "A web interface to your Urbit's command line (the dojo).",
      button: 'Install',
      color: '#9CA4B1',
      href: '/leap/search/direct/apps/~mister-dister-dozzod-dozzod/webterm',
      ship: '~mister-dister-dozzod-dozzod',
      desk: 'webterm'
    },
    {
      title: 'Groups',
      body: 'A suite of applications to communicate on Urbit',
      button: 'Install',
      color: '#D1DDD3',
      href: '/leap/search/direct/apps/~lander-dister-dozzod-dozzod/landscape',
      ship: '~lander-dister-dozzod-dozzod',
      desk: 'landscape'
    },
    {
      title: 'Bitcoin',
      body: ' A Bitcoin Wallet that lets you send and receive Bitcoin directly to and from other Urbit users',
      button: 'Install',
      color: '#F6EBDB',
      href: '/leap/search/direct/apps/~mister-dister-dozzod-dozzod/bitcoin',
      ship: '~mister-dister-dozzod-dozzod',
      desk: 'bitcoin'
    }
    // Commenting out until we have something real
    // {
    //   title: 'Debug',
    //   body: "Install a debugger. You can inspect your ship's internals using this interface",
    //   button: 'Install',
    //   color: '#E5E5E5',
    //   href: '/leap/search/direct/apps/~zod/debug'
    // }
    // {
    //   title: 'Build an app',
    //   body: 'You can instantly get started building new things on Urbit.  Just right click your Landscape and select “New App”',
    //   button: 'Learn more',
    //   color: '#82A6CA'
    // }
  ];
  if ('registerProtocolHandler' in window.navigator && !protocol) {
    cards.push({
      title: 'Open Urbit-Native Links',
      body: 'Enable your Urbit to open links you find in the wild',
      button: 'Enable Link Handler',
      color: '#82A6CA',
      href: '/leap/system-preferences/interface',
      desk: '',
      ship: ''
    });
  }

  return cards.filter((card) => {
    return !Object.values(vats).find(
      (vat) => getVatPublisher(vat) == card.ship && vat?.arak?.rail?.desk === card.desk
    );
  });
};

if ('registerProtocolHandler' in window.navigator) {
}

interface OnboardingCardProps {
  title: string;
  button: string;
  href: string;
  body: string;
  color: string;
  ship: string;
  desk: string;
}

const OnboardingCard = ({ title, button, href, body, color }: OnboardingCardProps) => (
  <div
    className="flex flex-col justify-between p-4 text-black bg-gray-100 space-y-2 rounded-xl"
    style={color ? { backgroundColor: color } : {}}
  >
    <div className="space-y-1">
      <h4 className="font-semibold text-black">{title}</h4>
      <p>{body}</p>
    </div>
    <Button as={Link} to={href} variant="primary" className="bg-gray-500">
      {button}
    </Button>
  </div>
);

interface OnboardingNotificationProps {
  unread?: boolean;
  lid: HarkLid;
}

export const OnboardingNotification = ({ unread = false, lid }: OnboardingNotificationProps) => {
  const theme = useCurrentTheme();
  const vats = useKilnState((s) => s.vats);
  const browserId = useBrowserId();
  const protocolHandling = useProtocolHandling(browserId);
  const cards = getCards(vats, protocolHandling);

  if (cards.length === 0 && !('time' in lid)) {
    useHarkStore.getState().archiveNote(
      {
        path: '/',
        place: {
          path: '/onboard',
          desk: window.desk
        }
      },
      lid
    );
    return null;
  }

  return (
    <section
      className={cn('notification space-y-2 text-black', unread ? 'bg-blue-100' : 'bg-gray-50')}
      aria-labelledby=""
    >
      <header id="system-updates-blocked" className="relative space-y-2">
        <div className="flex space-x-2">
          <span className="inline-block w-6 h-6 bg-gray-200 rounded" />
          <span className="font-semibold">System</span>
        </div>
        <div className="flex space-x-2">
          <h2 id="runtime-lag">Hello there, and welcome!</h2>
        </div>
      </header>
      <div className="grid sm:grid-cols-2 md:grid-cols-3 gap-4">
        {
          /* eslint-disable-next-line react/no-array-index-key */
          cards.map((card, i) => (
            <OnboardingCard
              key={i}
              {...card}
              color={theme === 'dark' ? getDarkColor(card.color) : card.color}
            />
          ))
        }
      </div>
    </section>
  );
};
