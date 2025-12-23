import React, { useState } from 'react';
import { Layout } from './components/Layout';
import { FocusView } from './views/FocusView';
import { PlanView } from './views/PlanView';
import { InboxView } from './views/InboxView';
import { useAppStore } from './store/AppStore';

function App() {
  const [currentView, setCurrentView] = useState<'focus' | 'plan' | 'inbox'>('focus');
  const { activeContextId, contexts } = useAppStore();

  const activeContext = contexts.find(c => c.id === activeContextId);

  // Apply theme variables dynamically
  React.useEffect(() => {
    if (activeContext) {
      const root = document.documentElement;
      root.style.setProperty('--active-theme-primary', `var(--theme-${activeContext.theme}-primary)`);
      root.style.setProperty('--active-theme-bg', `var(--theme-${activeContext.theme}-bg)`);
      root.style.setProperty('--active-theme-accent', `var(--theme-${activeContext.theme}-accent)`);
    }
  }, [activeContext]);

  const renderView = () => {
    switch (currentView) {
      case 'focus': return <FocusView />;
      case 'plan': return <PlanView />;
      case 'inbox': return <InboxView />;
      default: return <FocusView />;
    }
  };

  return (
    <Layout currentView={currentView} onNavigate={setCurrentView}>
      {renderView()}
    </Layout>
  );
}

export default App;
