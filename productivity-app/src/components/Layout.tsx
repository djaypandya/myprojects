import React from 'react';
import { Target, Calendar, Inbox } from 'lucide-react';

interface LayoutProps {
    children: React.ReactNode;
    currentView: 'focus' | 'plan' | 'inbox';
    onNavigate: (view: 'focus' | 'plan' | 'inbox') => void;
}

export const Layout: React.FC<LayoutProps> = ({ children, currentView, onNavigate }) => {
    return (
        <div className="app-container">
            <main style={{ flex: 1, padding: '1rem' }}>
                {children}
            </main>

            <nav className="bottom-nav">
                <button
                    className={`btn-ghost ${currentView === 'focus' ? 'active' : ''}`}
                    onClick={() => onNavigate('focus')}
                    style={{ color: currentView === 'focus' ? 'var(--active-theme-primary)' : 'var(--text-secondary)' }}
                >
                    <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: '4px' }}>
                        <Target size={24} />
                        <span style={{ fontSize: '12px', fontWeight: 500 }}>Focus</span>
                    </div>
                </button>

                <button
                    className={`btn-ghost ${currentView === 'plan' ? 'active' : ''}`}
                    onClick={() => onNavigate('plan')}
                    style={{ color: currentView === 'plan' ? 'var(--active-theme-primary)' : 'var(--text-secondary)' }}
                >
                    <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: '4px' }}>
                        <Calendar size={24} />
                        <span style={{ fontSize: '12px', fontWeight: 500 }}>Plan</span>
                    </div>
                </button>

                <button
                    className={`btn-ghost ${currentView === 'inbox' ? 'active' : ''}`}
                    onClick={() => onNavigate('inbox')}
                    style={{ color: currentView === 'inbox' ? 'var(--active-theme-primary)' : 'var(--text-secondary)' }}
                >
                    <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', gap: '4px' }}>
                        <Inbox size={24} />
                        <span style={{ fontSize: '12px', fontWeight: 500 }}>Inbox</span>
                    </div>
                </button>
            </nav>
        </div>
    );
};
