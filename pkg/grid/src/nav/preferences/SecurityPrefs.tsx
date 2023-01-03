import React, { useState } from 'react';
import classNames from 'classnames';
import { Button } from '../../components/Button';
import { Checkbox } from '../../components/Checkbox';

export const SecurityPrefs = () => {
  const [allSessions, setAllSessions] = useState(false);

  return (
    <>
      <h2 className="h3 mb-7">Security</h2>
      <div className="space-y-3">
        <section className={classNames('inner-section')}>
          <h3 className="flex items-center mb-2 h4">Logout</h3>
          <div className="flex flex-col justify-center flex-1 space-y-6">
            <Checkbox
              defaultChecked={false}
              checked={allSessions}
              onCheckedChange={() => setAllSessions((prev) => !prev)}
            >
              Log out of all sessions.
            </Checkbox>
            <form method="post" action="/~/logout">
              {allSessions && <input type="hidden" name="all" />}
              <Button>Logout</Button>
            </form>
          </div>
        </section>
      </div>
    </>
  );
};
